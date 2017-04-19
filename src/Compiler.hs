{-# LANGUAGE FlexibleInstances #-}

module Compiler where

import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Text.Casing
import Text.ParserCombinators.ReadP

import Parser
import Syntax

--------------------------------------------------------------------------------

type Pass a = (Env, a) -> Either String (Env, a)

compile :: (TypeCheck a, Adherence a, Normalize a) => Pass a
compile x
    = return x
  >>= pass0
  >>= pass1
  >>= pass2

pass0 :: TypeCheck a => Pass a
pass0 = typeChecks

pass1 :: Normalize a => Pass a
pass1 = Right . normalize

pass2 :: Adherence a => Pass a
pass2 = adheres

--------------------------------------------------------------------------------

class TypeCheck a where
  typeChecks :: a -> Either String a

instance TypeCheck (String, Type) where
  typeChecks x@(s, t) =
    let p = parserFor t
    in  fmap (const x) $ maybe (Left $ "expected: " ++ show t ++ ", but got value: " ++ s)
                               Right $ listToMaybe $ readP_to_S p s

instance TypeCheck (Value, Type) where
  typeChecks x@(BoolVal _, BoolType) = Right x
  typeChecks x@(NatVal n,  NatType)
    | n >= 0    = Right x
    | otherwise = Left $ "not a natural number: " ++ show n
  typeChecks x@(IntVal _,  IntType)  = Right x
  typeChecks x@(StrVal _,  StrType)  = Right x
  typeChecks (v, t)  =
    Left $ "expected: " ++ show t ++ ", but got value: " ++ show v

instance TypeCheck Stmt where
  typeChecks x@(Expect _ _)            = Right x
  typeChecks x@(Optional _ t Nothing)  = Right x
  typeChecks x@(Optional _ t (Just v)) = typeChecks (v, t) >>= const (Right x)

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
  normalize (Expect   n t)   = Expect   (normalize n) t
  normalize (Optional n t d) = Optional (normalize n) t d

instance Normalize Umwelt where
  normalize (Umwelt stmts) = Umwelt $ map normalize stmts

instance Normalize a => Normalize (Env, a) where
  normalize (env, x) = (env, normalize x)

--------------------------------------------------------------------------------

class Adherence a where
  adheres :: (Env, a) -> Either String (Env, a)

instance Adherence Stmt where
  adheres ([], Expect n _) = Left $ "expected env var " ++ show n
  adheres (((n, v):env'), s@(Expect n' t))
    | n == n'   = typeChecks (v, t) >> Right ([(n, v)], s)
    | otherwise = adheres (env', s)
  adheres ([], s@(Optional _ _ Nothing))  = Right ([], s)
  adheres ([], s@(Optional n _ (Just d))) = Right ([(n, show d)], s)
  adheres (((n, v):env'), o@(Optional n' t md))
    | n == n'   = typeChecks (v, t) >> Right ([(n, v)], o)
    | otherwise = adheres (env', o)

instance Adherence Umwelt where
  adheres (env, u@(Umwelt stmts)) = do
    envs <- fmap (map fst) $ sequence $ map adheres (zip (repeat env) stmts)
    Right (mconcat envs, u)
