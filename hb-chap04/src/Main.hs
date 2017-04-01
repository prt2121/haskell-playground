module Main where

data Mood = Blah | Woot deriving Show

-- 1. Mood
-- 2. Blah or Woot
-- 3.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

instance Eq Mood where
  (==) Woot Woot = True
  (==) Blah Blah = True
  (==) _ _       = False

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

ex1 = not True && True

ex2 x = not (x == 6)

ex3 = (1 * 2) > 5

ex4 = ["Merry"] > ["Happy"]

ex5 = ['1', '2', '3'] ++ "look at me!"

len :: [a] -> Integer
len [] = 0
len (x : xs) = 1 + len xs

main :: IO ()
main = do
  putStrLn "hello world"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs n = if n > 0 then n else (-1) * n

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

fun xs = x w 1
  where w = length xs
        x = (+)

id' x = x

head' = \(x : xs) -> x

fst' (a, b) = a

-- *Main> 6/3
-- 2.0
-- *Main> 6/ length [1,2,3]

-- <interactive>:9:1: error:
--     • No instance for (Fractional Int) arising from a use of ‘/’
--     • In the expression: 6 / length [1, 2, 3]
--       In an equation for ‘it’: it = 6 / length [1, 2, 3]
-- *Main> 6/ len [1,2,3]

-- <interactive>:10:1: error:
--     • No instance for (Fractional Integer) arising from a use of ‘/’
--     • In the expression: 6 / len [1, 2, 3]
--       In an equation for ‘it’: it = 6 / len [1, 2, 3
-- Main> fromIntegral 6 / (fromIntegral $ len [1,2,3])
-- 2.0
-- *Main> 2 + 3 == 5
-- True
-- *Main> let x = 5
-- *Main> x + 3 == 5
-- False
