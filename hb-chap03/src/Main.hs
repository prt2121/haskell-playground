module Reverse where

bang :: String -> String
bang = (++ "!")

why :: String -> Char
why = (!! 4)

awesome = dropWhile (/= 'a')

thirdLetter = (!! 2)

letterIndex :: Int -> Char
letterIndex = ("Curry is awesome!" !!)

-- You’re expected
-- only to slice and dice this particular string with take and drop.
-- Curry is awesome
-- awesome is Curry
rvrs :: String -> String
rvrs curryIsAwesome =
  (drop 9 curryIsAwesome)
  ++ " "
  ++ (take 2 . drop 6) curryIsAwesome
  ++ " "
  ++ (take 5 curryIsAwesome)

main :: IO ()
main = print $ rvrs "Curry is awesome"

-- concat [[1, 2, 3], [4, 5 , 6]]
-- [1,2,3,4,5,6]

-- *Main> ++ [1, 2, 3] [4, 5, 6]
-- <interactive>:4:1: error: parse error on input ‘++’
-- *Main> (++) [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]
-- *Main> (++) "hello" " world"
-- "hello world"
-- *Main> ["hello" ++ " world"]
-- ["hello world"]

-- *Main> 4 !! "hello"

-- <interactive>:9:6: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
--     • In the second argument of ‘(!!)’, namely ‘"hello"’
--       In the expression: 4 !! "hello"
--       In an equation for ‘it’: it = 4 !! "hello"
-- *Main> (!!) "hello" 4
-- 'o'
-- *Main> "hello" !! 4
-- 'o'
-- *Main> take "4 lovely"

-- <interactive>:12:6: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
--     • In the first argument of ‘take’, namely ‘"4 lovely"’
--       In the expression: take "4 lovely"
--       In an equation for ‘it’: it = take "4 lovely"
-- *Main> take 3 "awesome"
-- "awe"

-- concat [[1 * 6], [2 * 6], [3 * 6]]
-- [6,12,18]
-- *Main> "rain" ++ drop 2 "elbow"
-- "rainbow"
-- *Main> 10 * head [1, 2, 3]
-- 10
-- *Main> (take 3 "Julie") ++ (tail "yes")
-- "Jules"
-- *Main> concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]
-- [2,3,5,6,8,9]
