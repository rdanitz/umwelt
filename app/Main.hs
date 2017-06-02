module Main where

import System.Environment
import System.Process
import Text.ParserCombinators.ReadP

import Parser
import Syntax
import Compiler


p sy st = fst . head . (filter (null . snd)) $ readP_to_S sy st

proc' = proc "env" []

parsed = do
  input <- readFile "examples/predicates.env"
  return $ parse input

main :: IO ()
main = do
  env <- getEnvironment
  u <- parsed
  case compile (env, u) of
    Left err        -> putStrLn err
    Right (env', u') -> do
      createProcess $ proc' { env = Just env' }
      return ()

dev :: IO ()
dev = do
  env <- getEnvironment
  u <- parsed
  print u
  case compile (env, u) of
    Left err        -> putStrLn err
    Right (env', u') -> do
      print u'
      --createProcess $ proc' { env = Just env' }
      return ()
