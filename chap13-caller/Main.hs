module Main where

import Hello

main =
  do putStr "first name: "
     x1 <- getLine
     putStr "last name: "
     x2 <- getLine
     sayHello (x1 ++ " " ++ x2)
