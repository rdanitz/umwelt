{-# LANGUAGE FlexibleInstances #-}

module Compiler where

import Data.Char
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Text.Casing
import Text.ParserCombinators.ReadP

import Parser hiding (Pass, pass0, pass1)
import Syntax

--------------------------------------------------------------------------------

type Pass a = (Env, a) -> Either String (Env, a)

compile :: (TypeCheck a, Adherence a, Normalize a, Sat a) => Pass a
compile x
    = return x
  >>= pass0
  >>= pass1
  >>= pass2
  >>= pass3

pass0 :: TypeCheck a => Pass a
pass0 = typeChecks

pass1 :: Normalize a => Pass a
pass1 = Right . normalize

pass2 :: Adherence a => Pass a
pass2 = adheres

pass3 :: Sat a => Pass a
pass3 = satisfies

--------------------------------------------------------------------------------

class TypeCheck a where
  typeChecks :: a -> Either String a

instance TypeCheck (String, Type) where
  typeChecks x@(s, t@(EnumType vs)) =
    if s `elem` [s | EnumVal s <- vs]
    then Right x
    else Left $ "expected: " ++ show t ++ ", but got value: " ++ s
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
  typeChecks x@(e@(EnumVal v), t@(EnumType vs)) =
    if e `elem` vs
    then Right x
    else Left $ "expected: " ++ show t ++ ", but got value: " ++ show e
  typeChecks (v, t) =
    Left $ "expected: " ++ show t ++ ", but got value: " ++ show v

instance TypeCheck Stmt where
  typeChecks x@(Expect _ _ _)            = Right x -- XXX
  typeChecks x@(Optional _ t Nothing _)  = Right x -- XXX
  typeChecks x@(Optional _ t (Just v) _) = typeChecks (v, t) >>= const (Right x) -- XXX

instance TypeCheck Umwelt where
  typeChecks u@(Umwelt stmts) = case [e | Left e <- map typeChecks stmts] of
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

instance Normalize Umwelt where
  normalize (Umwelt stmts) = Umwelt $ map normalize stmts

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

instance Adherence Umwelt where
  adheres (env, u@(Umwelt stmts)) = do
    envs <- fmap (map fst) $ sequence $ map adheres (zip (repeat env) stmts)
    Right (mconcat envs, u)

--------------------------------------------------------------------------------

class Sat a where
  satisfies :: (Env, a) -> Either String (Env, a)

instance Sat (Value, Expr) where
  satisfies (env, x@(BoolVal True,  NullaryPredExpr "isTrue"))  = Right (env, x)
  satisfies (env, x@(BoolVal False, NullaryPredExpr "isFalse")) = Right (env, x)
  satisfies (env, x@(StrVal s, NullaryPredExpr s'))             = Left $
    "expected "

instance Sat (Value, Stmt) where
  satisfies (env, vs@(v, Expect n _ c)) = satisfies (env, vs)
    where Just v' = M.lookup n $ M.fromList env

instance Sat Umwelt where
  satisfies (env, u@(Umwelt stmts)) = Right (env, u)
