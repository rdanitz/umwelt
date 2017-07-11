module Parser where

import Prelude hiding (pred)
import Data.Char
import Control.Applicative ((<|>))
import Data.Maybe
import Text.ParserCombinators.ReadP

import Syntax

--------------------------------------------------------------------------------

type Pass = [String] -> [String]

parse :: String -> Umwelt
parse s =
  let s' = unlines . pass1 . pass0 . lines $ s
  in  case listToMaybe $ filter (null . snd) $ readP_to_S umwelt s' of
  Just (u, _) -> u
  Nothing     -> error "could not parse"

pass0 :: Pass
pass0 = map (takeWhile (/= '#'))

pass1 :: Pass
pass1 = filter (not . all isSpace)

-- utility

any'   = const True
spaces = skipMany1 $ satisfy isSpace
eol    = const () <$> char '\n'
parens  = between (char '(') (char ')')

parserFor :: Type -> ReadP Value
parserFor BoolType      = boolVal
parserFor NatType       = natVal
parserFor IntType       = intVal
parserFor StrType       = StrVal <$> (many $ satisfy any') -- XXX have to think about it a bit more
-- parserFor (EnumType vs) = enumVal

-- top level

umwelt :: ReadP Umwelt
umwelt
    = (Umwelt []) . fromJust . sequence . filter isJust -- XXX
  <$> many (choice [stmt, comment'])

stmt :: ReadP (Maybe Stmt)
stmt
    = Just
  <$> choice [ expect
             , optional'
             , alias
             ]
  <*  many (satisfy isSpace)
  <*  choice [eol, eof]

comment :: ReadP (Maybe Stmt)
comment
   = Nothing
  <$ char '#'
  <* skipMany1 (satisfy any')

comment'
    = id
  <$> comment

expect
    = Expect
  <$  char '!'
  <*  skipSpaces
  <*> identifier
  <*  skipSpaces
--   <*> (typeDecl <|> enumDecl)
  <*> typeDecl
  <*> option Nothing (Just <$ skipSpaces <*> constraint)

optional'
    = Optional
  <$  char '?'
  <*  skipSpaces
  <*> identifier
  <*  skipSpaces
--  <*> (typeDecl <|> enumDecl)
  <*> typeDecl
  <*> option Nothing (Just <$ skipSpaces <*> defaultDecl)
  <*> option Nothing (Just <$ skipSpaces <*> constraint)

defaultDecl
    = id
  <$  skipSpaces
  <*  string "~>"
  <*  skipSpaces
  <*> val

alias
    = Alias
  <$> identifier
  <*  skipSpaces
  <*  string ":="
  <*  skipSpaces
--  <*> (typeDecl <|> enumDecl)
  <*> type'
  <*> option Nothing (Just <$ skipSpaces <*> defaultDecl)
  <*> option Nothing (Just <$ skipSpaces <*> constraint)

-- names

identifier = (:) <$> satisfy begin <*> (many $ satisfy rest)
  where
    begin = (`elem` ['_', '-'] ++ ['a'..'z'])
    rest  = (`elem` ['_', '-'] ++ ['a'..'z'] ++ ['A'..'Z'])

-- types

type' = choice [type'', Compound <$> typeExpr]

type'' = f <$> choice [ identifier
                     , string "Bool"
                     , string "Nat"
                     , string "Int"
                     , string "String"
                     ]
  where
    f "Bool"   = BoolType
    f "Nat"    = NatType
    f "Int"    = IntType
    f "String" = StrType
    f x        = AliasType x

typeExpr
  = choice [ typeExpr'
           , TList <$> sepBy1 typeExpr' spaces
           ]

typeExpr'
  = choice [ lit
           , TChoice <$> expr <*  char '?'
           , TMany   <$> expr <*  char '*'
           , TMany1  <$> expr <*  char '+'
           , TVal    <$> val
           ]
  where
    lit  = TLit <$> type''
    expr = choice [lit, parens typeExpr]
  
    
-- Enums'
--   = many (   id
--          <$  skipSpaces
--          <*  char '|'
--          <*  skipSpaces
--          <*> enumVal
--          )
--
-- enumDecl
--     = f
--   <$  char ':'
--   <*  skipSpaces
--   <*> enumVal
--   <*> enums'
--   where
--   f v = EnumType . (v:)

-- enumVal = EnumVal <$> identifier

typeDecl
    = id
  <$  char ':'
  <*  skipSpaces
  <*> type'

-- values

boolVal = f <$> (string "true" <|> string "false")
  where
    f "true"  = BoolVal True
    f "True"  = BoolVal True
    f "1"     = BoolVal True
    f "false" = BoolVal False
    f "False" = BoolVal False
    f "0"     = BoolVal False

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
    = StrVal
  <$> between (char '"') (char '"') (many $ satisfy (/= '"'))

val = choice [ boolVal
             , natVal
             , intVal
             , strVal
             -- , enumVal
             ]

-- predicates

constraint
    = id
  <$  skipSpaces
  <*  string "with"
  <*  skipSpaces
  <*> pred

pred = Pred <$> (many $ satisfy (/= '\n'))
