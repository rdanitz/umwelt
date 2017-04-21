module Main where

import System.Environment
import System.Process

import Parser
import Syntax
import Compiler


p = proc "env" []

parse' = do
  input <- readFile "examples/real_world.env"
  return $ parse input

main :: IO ()
main = do
  env <- getEnvironment
  u <- parse'
  -- print u
  case compile (env, u) of
    Left err        -> putStrLn err
    Right (env', u') -> do
      -- print u'
      createProcess $ p { env = Just env' }
      return ()

dev :: IO ()
dev = do
  env <- getEnvironment
  u <- parse'
  print u
  case compile (env, u) of
    Left err        -> putStrLn err
    Right (env', u') -> do
      print u'
      createProcess $ p { env = Just env' }
      return ()
