module LeapYear where
--   Write a program that will take a year and report if it is a leap year.
--   http://exercism.io/exercises/haskell/leap/readme

isLeapYear :: Int -> Bool
isLeapYear n = (n `mod` 4) == 0 && (n `mod` 100) /= 0 || (n `mod` 400) == 0
