module Syntax where

import Prelude hiding (LT, EQ, GT, Ordering)
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

type Env = [(String, String)]

data Umwelt
  = Umwelt { aliases  :: [Stmt]
           , expected :: [Stmt]
           , optional :: [Stmt]
           }
  deriving Show

data Stmt
  = Expect String Type (Maybe Pred)
  | Optional String Type (Maybe Value) (Maybe Pred)
  | Alias String Type (Maybe Value) (Maybe Pred)
  deriving Show

data Type
  = AliasType String
  | BoolType
  | NatType
  | IntType
  | StrType
  | Compound TypeExpr
  deriving Eq

instance Show Type where
  show (AliasType x) = x
  show BoolType = "Bool"
  show NatType  = "Nat"
  show IntType  = "Int"
  show StrType  = "String"
  show (Compound expr) = show expr

data TypeExpr
  = TLit    Type
  | TChoice TypeExpr
  | TMany   TypeExpr
  | TMany1  TypeExpr
  | TVal    Value
  | TList   [TypeExpr]
  deriving (Eq, Show)

data Value
  = BoolVal Bool
  | NatVal  Integer
  | IntVal  Integer
  | StrVal  String
  deriving Eq

instance Show Value where
  show (BoolVal b) = show b
  show (NatVal  n) = show n
  show (IntVal  i) = show i
  show (StrVal  s) = s

newtype Pred = Pred String

instance Show Pred where
  show (Pred p) = p
