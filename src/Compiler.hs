{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Compiler where

import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Catch
import Data.Char
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Text.Casing
import Text.ParserCombinators.ReadP
import Language.Haskell.Interpreter hiding (typeChecks)
import Language.Haskell.Interpreter.Unsafe

import Parser hiding (Pass, pass0, pass1)
import Syntax

--------------------------------------------------------------------------------

type Pass a = (Env, a) -> Either String (Env, a)

compile :: (TypeCheck a, Adherence a, Normalize a, Sat (Env, a)) => (Env, a) -> IO (Env, a)
compile x = do
  (case a of
    Right y  -> return y
    Left err -> error err)
    >>= pass3
  where a   = return x
          >>= pass0
          >>= pass1
          >>= pass2

pass0 :: TypeCheck a => Pass a
pass0 = typeChecks

pass1 :: Normalize a => Pass a
pass1 = Right . normalize

pass2 :: Adherence a => Pass a
pass2 = adheres

pass3 :: Sat (Env, a) => (Env, a) -> IO (Env, a)
pass3 = satisfies

--------------------------------------------------------------------------------

class TypeCheck a where
  typeChecks :: a -> Either String a

instance TypeCheck (String, Type) where
  -- typeChecks x@(s, t@(EnumType vs)) =
  --   if s `elem` [s | EnumVal s <- vs]
  --   then Right x
  --   else Left $ "expected: " ++ show t ++ ", but got value: " ++ s
  typeChecks x@(s, t) =
    let p = parserFor t
    in  fmap (const x) $ maybe (Left $ "expected: " ++ show t ++ ", but got value: " ++ s)
                                Right $ listToMaybe $ readP_to_S p s

instance TypeCheck (Value, Type) where
  typeChecks x@(BoolVal _, BoolType) = Right x
  typeChecks x@(NatVal n,  NatType)
    | n >= 0    = Right x
    | otherwise = Left $ "not a natural number: " ++ show n
  typeChecks x@(IntVal _,  IntType)     = Right x
  typeChecks x@(StrVal _,  StrType)     = Right x
  -- typeChecks x@(e@(EnumVal v), t@(EnumType vs)) =
  --   if e `elem` vs
  --   then Right x
  --   else Left $ "expected one of: " ++ show t ++ ", but got value: " ++ show e
  typeChecks x@(v, AliasType _) = Right x -- XXX
  typeChecks (v, t) =
    Left $ "expected: " ++ show t ++ ", but got value: " ++ show v

instance TypeCheck Stmt where
  typeChecks x@(Expect _ _ _) = Right x -- XXX

  typeChecks x@(Optional _ t Nothing _)  = Right x -- XXX
  typeChecks x@(Optional _ t (Just v) _) = typeChecks (v, t) >>= const (Right x) -- XXX

  typeChecks x@(Alias _ _ _ _) = Right x -- XXX

instance TypeCheck Umwelt where
  typeChecks u@(Umwelt aliases stmts) = case [e | Left e <- map typeChecks stmts] of -- XXX
    []    -> Right u
    err:_ -> Left err

instance TypeCheck a => TypeCheck (Env, a) where
  typeChecks (env, x) = typeChecks x >>= const (Right (env, x))

--------------------------------------------------------------------------------

class Normalize a where
  normalize :: a -> a

instance Normalize String where
  normalize = screamingSnake

instance Normalize Stmt where
  normalize (Expect   n t c)   = Expect   (normalize n) t c -- XXX
  normalize (Optional n t d c) = Optional (normalize n) t d c -- XXX
  normalize x@(Alias _ _ _ _)  = x

instance Normalize Umwelt where
  normalize (Umwelt aliases stmts) = Umwelt [] $ map normalize stmts -- XXX

instance Normalize a => Normalize (Env, a) where
  normalize (env, x) = (env, normalize x)

--------------------------------------------------------------------------------

class Adherence a where
  adheres :: (Env, a) -> Either String (Env, a)

instance Adherence Stmt where
  adheres ([], Expect n _ _) = Left $ "expected environment variable " ++ show n -- XXX
  adheres ((n, v):env', s@(Expect n' t _)) -- XXX
    | n == n'   = typeChecks (v, t) >> Right ([(n, v)], s)
    | otherwise = adheres (env', s)

  adheres ([], s@(Optional _ _ Nothing _))  = Right ([], s) -- XXX
  adheres ([], s@(Optional n _ (Just d) _)) = Right ([(n, show d)], s) -- XXX
  adheres ((n, v):env', o@(Optional n' t md _)) -- XXX
    | n == n'   = typeChecks (v, t) >> Right ([(n, v)], o)
    | otherwise = adheres (env', o)

  adheres (env, s@(Alias _ _ _ _)) = Right ([], s) -- XXX

instance Adherence Umwelt where
  adheres (env, u@(Umwelt aliases stmts)) = do -- XXX
    envs <- fmap (map fst) $ sequence $ map adheres (zip (repeat env) stmts)
    Right (mconcat envs, u)

--------------------------------------------------------------------------------

eval' :: (Control.Monad.Catch.MonadMask m, MonadIO m) => Value -> Pred -> m ()
eval' v p = do
  r <- runInterpreter $ do
    loadModules modules
    setImportsQ imports
    eval expr
  case r of
    Right "True"  -> return ()
    Right "False" -> error $ "\npredicate not satisfied: " ++ show p ++
                             "\nvalue: " ++ show v
    Right x       -> error $ "unexpected predicate evaluation: " ++ show x
    Left (WontCompile errs) -> error $ "won't compile:\n" ++ concatMap errMsg errs
    Left err -> error $ show err
  where modules = [ "src/Predicates.hs" ]
        imports = [ ("Prelude", Just "P")
                  , ("Predicates", Nothing)
                  ]
        expr = v' ++ " & " ++ show p
        v' = case v of
          StrVal s -> show s
          _        -> show v


class Sat a where
  satisfies :: (MonadMask m, MonadIO m) => a -> m a

instance Sat (Value, Pred) where
  satisfies (v, p) = eval' v p >> return (v, p)

instance Sat (Env, Stmt) where
  satisfies (env, s@(Expect _ _ Nothing)) = return (env, s)
  satisfies ((n, s):env', stmt@(Expect n' t (Just p)))
    | n == n'   = satisfies (v, p) >> return ([], stmt)
    | otherwise = satisfies (env', stmt)
    where p' = parserFor t
          Just (v, _) = listToMaybe $ filter (null . snd) $ readP_to_S p' s

  satisfies (env, s@(Optional _ _ _ Nothing)) = return ([], s)
  satisfies ((n, s):env', stmt@(Optional n' t _ (Just p)))
    | n == n'   = satisfies (v, p) >> return ([], stmt)
    | otherwise = satisfies (env', stmt)
    where p' = parserFor t
          Just (v, _) = listToMaybe $ filter (null . snd) $ readP_to_S p' s

  satisfies (env, s@(Alias _ _ _ _)) = return (env, s)

  satisfies x@([], _) = return x

instance Sat (Env, Umwelt) where
  satisfies (env, u@(Umwelt aliases stmts)) = do -- XXX
    mapM_ satisfies (zip (repeat env) stmts)
    return (env, u)
