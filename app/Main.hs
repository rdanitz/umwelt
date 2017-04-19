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
  case compile (env, u) of
    Left err        -> putStrLn err
    Right (env', _) -> do
      createProcess $ p { env = Just env' }
      return ()
