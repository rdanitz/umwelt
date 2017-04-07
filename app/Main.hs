module Main where

import Parser
import Syntax

input = readFile "examples/simple.env"

main :: IO ()
main = do
  input <- readFile "examples/simple.env"
  let out = parse input
  print out
