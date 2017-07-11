module Syntax where

import Prelude hiding (LT, EQ, GT, Ordering)
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

type Env = [(String, String)]

data Umwelt
  = Umwelt { aliases :: [Stmt]
           , stmts   :: [Stmt]
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
  -- | EnumType [Value]
  | Compound TypeExpr
  deriving (Eq, Show)

-- instance Show Type where
--   show (AliasType x) = x
--   show BoolType = "Bool"
--   show NatType  = "Nat"
--   show IntType  = "Int"
--   show StrType  = "String"
--   -- show (EnumType vals) = intercalate ", " [show v | EnumVal v <- vals]
--   show (Compound expr) = show expr
--   --  = intercalate ", " $ map show exprs

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
  -- | EnumVal String
  deriving Eq

instance Show Value where
  show (BoolVal b) = show b
  show (NatVal  n) = show n
  show (IntVal  i) = show i
  show (StrVal  s) = s
  -- show (EnumVal e) = show e

newtype Pred = Pred String

instance Show Pred where
  show (Pred p) = p
