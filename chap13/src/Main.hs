module Main where

import Hello (sayHello)

main :: IO()
main = do
  name <- getLine
  sayHello name
