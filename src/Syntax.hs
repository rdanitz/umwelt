module Syntax where

import Prelude hiding (LT, EQ, GT, Ordering)
import Data.Maybe

--------------------------------------------------------------------------------

type Env = [(String, String)]

newtype Umwelt
  = Umwelt [Stmt]
  deriving Show

data Stmt
  = Expect String Type (Maybe Expr)
  | Optional String Type (Maybe Value) (Maybe Expr)
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
  deriving (Eq, Show)

data Expr
  = NullaryPredExpr String
  | UnaryPredExpr String Value
  | OrdExpr  Ordering Value
  | BoolExpr BoolOp [Expr]
  deriving Show

data BoolOp = And | Or | Not
  deriving Show

data Ordering
  = LT
  | LTE
  | GT
  | GTE
  | EQ
  deriving Show
