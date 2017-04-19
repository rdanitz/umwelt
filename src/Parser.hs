module Parser where

import Control.Applicative ((<|>))
import Data.Maybe
import Text.ParserCombinators.ReadP

import Syntax

--------------------------------------------------------------------------------

parse :: String -> Umwelt
parse s = case listToMaybe $ readP_to_S umwelt s of
  Just (u, _) -> u
  Nothing     -> error "could not parse"

-- utiity

parserFor :: Type -> ReadP Value
parserFor BoolType = boolVal
parserFor NatType  = natVal
parserFor IntType  = intVal
parserFor StrType  = StrVal <$> (many $ satisfy any') -- XXX have to think about it a bit more

any' = const True
eol = satisfy (== '\n')

-- top level

umwelt = do
  stmts <- many1 $ choice [stmt, comment, eol >> return Nothing]
  eof
  let Just stmts' = sequence . filter isJust $ stmts
  return $ Umwelt stmts'

space = satisfy (== ' ')
spaces = skipMany space

stmt = do
  s <- choice [expect, optional']
  spaces
  optional comment
  eol
  return $ Just s

comment = do
  char '#'
  skipMany1 $ satisfy any'
  return Nothing

expect = do
  char '!'
  spaces
  v <- var
  spaces
  t <- typeDecl
  return $ Expect v t

optional' = do
  char '?'
  spaces
  v  <- var
  spaces
  t  <- typeDecl
  spaces
  md <- option Nothing (Just <$> defaultDecl)
  return $ Optional v t md

defaultDecl = do
  string "~>"
  spaces
  val

-- names

isVarChar = (`elem` ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0'..'9'] ++ ['_', '-'])
var = many1 $ satisfy isVarChar

type' = choice [ string "Bool"
               , string "Nat"
               , string "Int"
               , string "String"
               ]

-- types

typeDecl = do
  char ':'
  spaces
  t <- type'
  return $ case t of
    "Bool"   -> BoolType
    "Nat"    -> NatType
    "Int"    -> IntType
    "String" -> StrType

-- values

boolVal = do
  v <- string "true" <|> string "false"
  case v of
    "true"  -> return $ BoolVal True
    "false" -> return $ BoolVal False

isDigit = (`elem` ['0'..'9'])

natVal = do
  n <- many1 $ satisfy isDigit
  return $ NatVal (read n :: Integer)

intVal = do
  ms <- option Nothing (Just <$> char '-')
  NatVal n <- natVal
  return $ case ms of
    Just '-' -> IntVal (-n)
    Nothing -> IntVal n

strVal = do
  char '"'
  v <- many $ satisfy any'
  char '"'
  return $ StrVal v

val = choice [boolVal, natVal, intVal, strVal]
