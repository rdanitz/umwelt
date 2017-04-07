module Parser where

import Data.Maybe
import Text.ParserCombinators.ReadP

import Syntax

--------------------------------------------------------------------------------

parse = sequence . filter isJust . fst . head . readP_to_S umwelt

space = satisfy (== ' ')
spaces = skipMany space

any' = const True

comment = do
  char '#'
  skipMany1 $ satisfy any'
  return Nothing

eol = satisfy (== '\n')

isVarChar = (`elem` ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0'..'9'] ++ ['_', '-'])
var = many1 $ satisfy isVarChar

type' = choice [ string "Bool"
               , string "Nat"
               , string "Int"
               , string "String"
               ] 

typeDecl = do
  char ':'
  spaces 
  t <- type'
  return $ case t of
    "Bool"   -> BoolT
    "Nat"    -> NatT
    "Int"    -> IntT
    "String" -> StrT

expect = do
  char '!'
  spaces
  v <- var
  spaces
  t <- typeDecl
  return $ Expect v t

isDigit = (`elem` ['0'..'9'])

natVal = do
  n <- many1 $ satisfy isDigit
  return $ NatV (read n :: Integer)

intVal = do
  ms <- option Nothing (Just <$> char '-')
  NatV n <- natVal
  return $ case ms of
    Just '-' -> IntV (-n)
    Nothing -> IntV n 

strVal = do
  char '"'
  v <- many $ satisfy any'
  char '"'
  return $ StrV v

val = choice [natVal, intVal, strVal]

defaultDecl = do
  string "~>"
  spaces
  val

optional' = do
  char '?'
  spaces
  v  <- var
  spaces
  t  <- typeDecl
  spaces
  md <- option Nothing (Just <$> defaultDecl)
  return $ Optional v t md

stmt = do
  s <- choice [expect, optional']
  spaces
  optional comment
  eol
  return $ Just s

umwelt = do
  u <- many1 $ choice [stmt, comment, eol >> return Nothing]
  eof
  return u
