{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections     #-}

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
  typeChecks x@(s, t) =
    let p = parserFor t
    in  fmap (const x) $ maybe (Left $ "expected: " ++ show t ++ ",\n but got value: " ++ s)
                                Right $ listToMaybe $ readP_to_S p s

instance TypeCheck (Umwelt, Value, Type) where
  typeChecks x@(u, BoolVal _, BoolType) = Right x
  typeChecks x@(u, NatVal n,  NatType)
    | n >= 0    = Right x
    | otherwise = Left $ "not a natural number: " ++ show n
  typeChecks x@(u, IntVal _,  IntType)  = Right x
  typeChecks x@(u, StrVal _,  StrType)  = Right x

  typeChecks x@(u, v, AliasType n) = case [t | Alias n t _ _ <- aliases u] of
    t:_ -> typeChecks (u, v, t) >> Right x -- XXX
    []   -> Left $ "type alias not defined: " ++ show n
  typeChecks x@(u, v, Compound expr) = typeChecks (u, v, expr) >> Right x

  typeChecks (u, v, t) =
    Left $ "expected: " ++ show t ++ ",\n but got value: " ++ show v

instance TypeCheck (Umwelt, Value, TypeExpr) where
  typeChecks x@(u, v, TLit    t)    = typeChecks (u, v, t)    >> Right x
  typeChecks x@(u, v, TChoice expr) = typeChecks (u, v, expr) >> Right x -- XXX
  typeChecks x@(u, v, TMany   expr) = typeChecks (u, v, expr) >> Right x -- XXX
  typeChecks x@(u, v, TMany1  expr) = typeChecks (u, v, expr) >> Right x -- XXX
  typeChecks x@(u, v, TVal    v')   =
    if v == v'
    then Right x
    else Left $ "expected value: " ++ show v ++ ",\n but got: " ++ show v'

  typeChecks (u, v, TList []) = Left $ "unconsumed value: " ++ show v
  typeChecks x@(u, v, TList (e:es)) = do
    typeChecks (u, v, e)
    typeChecks (u, v, TList es)
    Right x -- XXX
  -- typeChecks _ = error "not implemented"

instance TypeCheck (Umwelt, Stmt) where
  typeChecks x@(u, Expect _ _ _) = Right x -- XXX

  typeChecks x@(u, Optional _ t Nothing  _) = Right x -- XXX
  typeChecks x@(u, Optional _ t (Just v) _) = typeChecks (u, v, t) >>= const (Right x) -- XXX

  typeChecks x@(u, Alias _ t Nothing  _) = Right x -- XXX
  typeChecks x@(u, Alias _ t (Just v) _) = typeChecks (u, v, t) >>= const (Right x) -- XXX

instance TypeCheck Umwelt where
  typeChecks u@(Umwelt a e o) = case [x | Left x <- map (typeChecks . (u,)) a]
                                  ++ [x | Left x <- map (typeChecks . (u,)) e]
                                  ++ [x | Left x <- map (typeChecks . (u,)) o] of
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
  normalize (Umwelt a e o) = Umwelt (map normalize a) (map normalize e) (map normalize o)

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
  adheres (env, u@(Umwelt a e o)) = do
    envs <- f a >> f e >> f o
    Right (mconcat envs, u)
    where f x = fmap (map fst) $ sequence $ map adheres (zip (repeat env) x)

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
  satisfies (env, u@(Umwelt a e o)) = do
    mapM_ satisfies (zip (repeat env) (a ++ e ++ o))
    return (env, u)
