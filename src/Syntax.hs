module Syntax where

import Prelude hiding (LT, EQ, GT, Ordering)
import Data.Maybe

--------------------------------------------------------------------------------

type Env = [(String, String)]

newtype Umwelt
  = Umwelt [Stmt]
  deriving Show

data Stmt
  = Expect String Type (Maybe Pred)
  | Optional String Type (Maybe Value) (Maybe Pred)
  deriving Show

data Type
  = BoolType
  | NatType
  | IntType
  | StrType
  | EnumType [Value]
  deriving (Eq, Show)

data Value
  = BoolVal Bool
  | NatVal  Integer
  | IntVal  Integer
  | StrVal  String
  | EnumVal String
  deriving Eq

instance Show Value where
  show (BoolVal b) = show b
  show (NatVal  n) = show n
  show (IntVal  i) = show i
  show (StrVal  s) = s
  show (EnumVal e) = show e

newtype Pred = Pred String

instance Show Pred where
  show (Pred p) = p
