module Main where

import System.Environment
import System.Process
import Text.ParserCombinators.ReadP

import Parser
import Syntax
import Compiler
import Predicates

p sy st = fst . head . (filter (null . snd)) $ readP_to_S sy st

proc' = proc "env" []

parsed = do
  input <- readFile "examples/predicates.umwelt"
  return $ parse input

main :: IO ()
main = do
  env <- getEnvironment
  u <- parsed
  (env', u') <- compile (env, u)
  createProcess $ proc' { env = Just env' }
  return ()

dev :: IO ()
dev = do
  env <- getEnvironment
  u <- parsed
  (env', u') <- compile (env, u)
  print u'
  createProcess $ proc' { env = Just env' }
  return ()
