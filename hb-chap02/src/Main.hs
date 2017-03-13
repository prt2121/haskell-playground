-- module Main where

-- Given the following lines of code as they might appear in a
-- source file, how would you change them to use them directly in
-- the REPL?

half x = x / 2

square x = x * x

f :: Int -> String
f x
  | x `mod` 15 == 0 = "Yo"
  | x `mod` 3 == 0  = "Fizz"
  | x `mod` 5 == 0  = "Buzz"
  | otherwise       = "Lol"

-- half' :: Fractional a => a

-- let square' x = x * x

sayHello :: String -> IO ()
sayHello x = putStrLn ("hello, " ++ x ++ "!")

-- half x = x / 2
-- let half x = x / 2

-- square x = x * x
-- let square x = x * x

area r = 3.14 * r * r

area' r = pi * r * r

-- *Main> let x = 5 in x
-- 5
-- *Main> let x = 5 in x * x
-- 25
-- *Main> let x = 5; y = 6 in x * y
-- 30
-- *Main> let x = 3; y = 1000 in x + 3
-- 6

z = 7

x = y ^ 2

waxOn = x * 5

y = z + 8

-- 10 + waxOn = 1135
-- (-) 15 waxOn = -1110
-- (-) waxOn 15 = 1110
-- tripple waxOn
-- 3375

waxOn' :: Integer
waxOn' = x * 5
  where
    x = y ^ 2
    y = z + 8
    z = 7

-- waxOff = tripple

-- tripple x = x * 3

main :: IO ()
main = do
  putStrLn "hello world"
