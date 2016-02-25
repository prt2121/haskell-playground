module SumOfMultiples where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples ls m = sum [n | n <- [1..(m - 1)], any ((0 ==) . (n `mod`)) ls]

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault m = sumOfMultiples [3,5] m
