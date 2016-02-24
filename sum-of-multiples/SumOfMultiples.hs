module SumOfMultiples where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples ls m = sum [n | n <- [1..(m - 1)], any (\x -> n `mod` x == 0) ls]

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault m = sumOfMultiples [3,5] m
