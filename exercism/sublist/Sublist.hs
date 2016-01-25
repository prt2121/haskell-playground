module Sublist where
-- http://exercism.io/exercises/haskell/sublist

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist xs ys
  | equal xs ys     = Equal
  | sublist' xs ys  = Sublist
  | superList xs ys = Superlist
  | otherwise       = Unequal

equal :: (Eq a) => [a] -> [a] -> Bool
equal xs ys = length xs == length ys && and (zipWith (==) xs ys)

unequal :: (Eq a) => [a] -> [a] -> Bool
unequal = (not .) . equal

sublist' :: (Eq a) => [a] -> [a] -> Bool
sublist' [] _                = True
sublist' _  []               = False
sublist' [x] [y]             = x == y
sublist' l1@(x:_) l2@(y:ys) = if length l1 > length l2
                                  then False
                                  else if x == y
                                       then if (takeEqual l1 l2) == l1 then True else sublist' l1 ys
                                       else sublist' l1 ys

superList :: (Eq a) => [a] -> [a] -> Bool
superList xs ys = sublist' ys xs

dropPrefix :: (Eq a) => [a] -> [a] -> [a]
dropPrefix xs (y:_) = dropWhile (/= y) xs
dropPrefix _ [] = []

takeEqual :: (Eq a) => [a] -> [a] -> [a]
takeEqual [] _ = []
takeEqual _ [] = []
takeEqual (x:xs) (y:ys) = if x == y then x : (takeEqual xs ys) else []
