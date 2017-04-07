{-# LANGUAGE ExistentialQuantification #-}

module Syntax where

import Data.Maybe

--------------------------------------------------------------------------------

data Stmt =
    Expect String Type
  | Optional String Type (Maybe Value)

instance Show Stmt where
  show (Expect s t)     = s ++ ": " ++ show t
  show (Optional s t d) = s ++ ": " ++ show t ++ (fromMaybe "" $ fmap ((" ~> " ++) . show) d)

data Type =
    BoolT
  | NatT
  | IntT
  | StrT

instance Show Type where
  show BoolT = "Bool"
  show NatT  = "Nat"
  show IntT  = "Int"
  show StrT  = "String"

data Value =
    BoolV Bool
  | NatV Integer
  | IntV Integer
  | StrV String

instance Show Value where
  show (BoolV x) = show x
  show (NatV  x) = show x
  show (IntV  x) = show x
  show (StrV  x) = show x
