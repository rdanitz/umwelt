module Parser where

import Data.Char
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

any' = const True

parserFor :: Type -> ReadP Value
parserFor BoolType      = boolVal
parserFor NatType       = natVal
parserFor IntType       = intVal
parserFor StrType       = StrVal <$> (many $ satisfy any') -- XXX have to think about it a bit more
parserFor (EnumType vs) = enumVal

-- top level

umwelt
  =   Umwelt . fromJust . sequence . filter isJust
  <$> many1 (choice [stmt, comment'])
  <*  eof

stmt
  =   Just
  <$> choice [expect, optional']
  <*  skipSpaces

comment
  =  Nothing
  <$ char '#'
  <* skipMany1 (satisfy any')

comment'
  =   id
  <$> comment

expect
  =   Expect
  <$  char '!'
  <*  skipSpaces
  <*> var
  <*  skipSpaces
  <*> (typeDecl <|> enumDecl)

optional'
  =   Optional
  <$  char '?'
  <*  skipSpaces
  <*> var
  <*  skipSpaces
  <*> (typeDecl <|> enumDecl)
  <*  skipSpaces
  <*> option Nothing (Just <$> defaultDecl)

defaultDecl
  =   id
  <$  string "~>"
  <*  skipSpaces
  <*> val

-- names

isIdChar c = isAlphaNum c || c `elem` ['_', '-']
var = many1 $ satisfy isIdChar

type' = choice [ string "Bool"
               , string "Nat"
               , string "Int"
               , string "String"
               ]

-- types

enums'
  = many (  enumVal
         <* skipSpaces
         <* char '|'
         <* skipSpaces
         )

enumDecl
  = f
  <$  char ':'
  <*  skipSpaces
  <*> enums'
  <*> enumVal
  where
  f vs v = EnumType $ vs ++ [v]

typeDecl
  =   f
  <$  char ':'
  <*  skipSpaces
  <*> type'
  where
  f "Bool"   = BoolType
  f "Nat"    = NatType
  f "Int"    = IntType
  f "String" = StrType

-- values

boolVal = f <$> (string "true" <|> string "false")
  where
  f "true"  = BoolVal True
  f "false" = BoolVal False

natVal
  = NatVal . read
  <$> many1 (satisfy isDigit)

intVal
  = f
  <$> option Nothing (Just <$> char '-')
  <*> natVal
  where
  f Nothing    (NatVal n) = IntVal n
  f (Just '-') (NatVal n) = IntVal (-n)

strVal
  =   StrVal
  <$> between (char '"') (char '"') (many $ satisfy any')

isEnumChar = isIdChar
enumVal = EnumVal <$> (many1 $ satisfy isEnumChar)

val = choice [boolVal, natVal, intVal, strVal, enumVal]
