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

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x                     = MinMax x
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
  lit x                     = Mod7 $ mod x 7
  add (Mod7 x) (Mod7 y)     = Mod7 $ mod (x+y) 7
  mul (Mod7 x) (Mod7 y)     = Mod7 $ mod (x*y) 7

testExp :: Expr a => Maybe a
testExp     = parseExp lit add mul "(3 * -4) + 5"
testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer
testBool :: Maybe Bool
testBool    = testExp :: Maybe Bool
testMM :: Maybe MinMax
testMM      = testExp :: Maybe MinMax
testSat :: Maybe Mod7
testSat     = testExp :: Maybe Mod7

main :: IO ()
main = do
  putStrLn "hello world"
