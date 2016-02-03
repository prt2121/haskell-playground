{-# OPTIONS_GHC -Wall #-}
-- http://www.cis.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf
module Main where

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n)      = n
eval (Add e1 e2)  = eval e1 + eval e2
eval (Mul e1 e2)  = eval e1 * eval e2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . (parseExp Lit Add Mul)

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

main :: IO ()
main = do
  putStrLn "hello world"
