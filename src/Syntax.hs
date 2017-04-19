{-# LANGUAGE ExistentialQuantification #-}

module Syntax where

import Data.Maybe

--------------------------------------------------------------------------------

type Env = [(String, String)]

newtype Umwelt
  = Umwelt [Stmt]
  deriving Show

data Stmt
  = Expect String Type
  | Optional String Type (Maybe Value)
  deriving Show

data Type
  = BoolType
  | NatType
  | IntType
  | StrType
  deriving (Eq, Show)

data Value
  = BoolVal Bool
  | NatVal  Integer
  | IntVal  Integer
  | StrVal  String
  deriving (Eq, Show)
