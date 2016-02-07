module Sublist where
-- http://exercism.io/exercises/haskell/sublist

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist xs ys
  | xs == ys        = Equal
  | sublist' xs ys  = Sublist
  | superList xs ys = Superlist
  | otherwise       = Unequal

sublist' :: (Eq a) => [a] -> [a] -> Bool
sublist' [] _                = True
sublist' _  []               = False
sublist' l1@(x:_) l2@(y:ys)  =
  length l1 <= length l2 && ((x == y && (subEqual l1 l2 || sublist' l1 ys)) || (x /= y && sublist' l1 ys))

superList :: (Eq a) => [a] -> [a] -> Bool
superList = flip sublist'

subEqual :: (Eq a) => [a] -> [a] -> Bool
subEqual [] _ = True
subEqual _ [] = False
subEqual (x:xs) (y:ys) = x == y && subEqual xs ys
