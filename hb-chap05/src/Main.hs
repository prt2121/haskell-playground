{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

not' :: Bool -> Bool
not' True = False
not' _    = True

length' :: Foldable f => f a -> Int
length' = foldr (\_ x -> x + 1) 0

concat' :: Foldable f => f [a] -> [a]
concat' = foldr (++) []

head' :: [a] -> a
head' (h : _) = h

-- (<) :: Ord a => a -> a -> Bool


-- CHAPTER 5. TYPES
-- Exercises: Type Arguments

-- a -> a -> a -> a
-- Char -> Char -> Char

-- a -> b -> c -> b
-- Char
-- *Main> let g :: a -> b -> c -> b; g = undefined
-- *Main> let x :: Int; x = undefined
-- *Main> let y :: Char; y = undefined
-- *Main> let z :: String; z = undefined
-- *Main> :t g 0 'c' "woot"
-- g 0 'c' "woot" :: Char

-- (Num a, Num b) => a -> b -> b
-- Integer

-- (Ord a, Eq b) => a -> b -> a
-- [Char]

-- *Main> let kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- *Main> :t kessel 1 2
-- kessel 1 2 :: (Num a, Ord a) => a
-- *Main> :t kessel 1 (2 :: Integer)
-- kessel 1 (2 :: Integer) :: (Num a, Ord a) => a
-- *Main> :t kessel (1 :: Integer) 2
-- kessel (1 :: Integer) 2 :: Integer

-- value of type [a] is
-- a list whose elements are all of some type 𝑎

-- [[a]] -> [a] can take a list of strings as an argument

-- [a] -> Int -> a returns one element of type 𝑎 from a list

-- A function of type (a, b) -> a takes a tuple argument and returns the first value

determine01 :: Num a => a
determine01 = (* 9) 6

determine02 :: Num a => (a, String)
determine02 = head [(0,"doge"),(1,"kitteh")]

determine03 :: (Integer, [Char])
determine03 = head [(0 :: Integer ,"doge"),(1,"kitteh")]

determine04 :: Bool
determine04 = if False then True else False

determine05 :: Int
determine05 = length [1, 2, 3, 4, 5]

determine06 :: Bool
determine06 = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- *Main> let y = x + 5
-- *Main> let x = 5
-- *Main> let y = x + 5
-- *Main> let w = y * 10
-- *Main> :t w
-- w :: Num a => a
-- *Main> let f = 4 / y
-- *Main> :t f
-- f :: Fractional a => a

bigNum = (^) 5
wahoo = bigNum $ 10

x = print
y = print "woohoo!"
z = x "hello world"

a = (+)
b = 5
c = a 10
d = c 200

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

main :: IO ()
main = do
  putStrLn "hello world"
