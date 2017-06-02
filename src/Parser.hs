module Parser where

import Prelude hiding (LT, EQ, GT, Ordering)
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
spaces = skipMany $ satisfy isSpace
eol    = const () <$> char '\n'
parens  = between (char '(') (char ')')

parserFor :: Type -> ReadP Value
parserFor BoolType      = boolVal
parserFor NatType       = natVal
parserFor IntType       = intVal
parserFor StrType       = StrVal <$> (many $ satisfy any') -- XXX have to think about it a bit more
parserFor (EnumType vs) = enumVal

-- top level

umwelt
    = Umwelt . fromJust . sequence . filter isJust
  <$> many (choice [stmt, comment'])

stmt
    = Just
  <$> choice [expect, optional']
  <*  spaces
  <*  choice [eol, eof]

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
  -- <*> (typeDecl <|> enumDecl) XXX
  <*> typeDecl
  <*> option Nothing (Just <$ skipSpaces <*> constraint)

optional'
    = Optional
  <$  char '?'
  <*  skipSpaces
  <*> identifier
  <*  skipSpaces
  -- <*> (typeDecl <|> enumDecl) -- XXX
  <*> typeDecl
  <*> option Nothing (Just <$ skipSpaces <*> defaultDecl)
  <*> option Nothing (Just <$ skipSpaces <*> constraint)

defaultDecl
    = id
  <$  spaces
  <*  string "~>"
  <*  skipSpaces
  <*> val

-- names

isIdChar c = isAlphaNum c || c `elem` ['_', '-']
identifier = many1 $ satisfy isIdChar

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
    = f
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
    = StrVal
  <$> between (char '"') (char '"') (many $ satisfy any')

isEnumChar = isIdChar
enumVal = EnumVal <$> (many1 $ satisfy isEnumChar)

val = choice [ boolVal
             , natVal
             , intVal
             , strVal
             -- , enumVal XXX
             ]

-- predicates

constraint
    = id
  <$  skipSpaces
  <*  string "with"
  <*  skipSpaces
  <*> expr

expr = choice [ nullaryPredExpr
              , unaryPredExpr
              , boolExpr
              ]

nullaryPredExpr
   = NullaryPredExpr
 <$> identifier

unaryPredExpr
   = UnaryPredExpr
 <$> identifier
 <*  spaces
 <*> val

ordExpr
    = OrdExpr
  <$> ordering
  <*  skipSpaces
  <*> val

boolExpr = choice [ andExpr
                  , orExpr
                  , notExpr
                  ]

binExpr' op
    = (,)
  <$> expr
  <*  spaces
  <*  string (f op)
  <*  spaces
  <*> expr
  where f And = "&&"
        f Or  = "||"

binExpr op =
      BoolExpr op
  <$> parens (f <$> binExpr' op)
  where f (x, y) = [x, y]

andExpr = binExpr And
orExpr  = binExpr Or
notExpr
    = BoolExpr Not
  <$> (pure <$> (   id
                <$  string "!"
                <*  spaces
                <*> expr
                ))

ordering = f <$> choice [ string "<"
                        , string "<="
                        , string "=="
                        , string ">"
                        , string ">="
                        ]
  where
    f "<"  = LT
    f "<=" = LTE
    f "==" = EQ
    f ">"  = GT
    f ">=" = GTE
