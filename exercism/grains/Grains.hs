-- http://exercism.io/exercises/haskell/grains
module Grains where

square :: Int -> Integer
square n = ls !! (n-1)

sum' :: [Integer] -> Integer
sum' = foldr (+) 0

ls :: [Integer]
ls = iterate (*2) 1

total :: Integer
total = sum' $ take 64 ls
