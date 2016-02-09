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
sublist' l1 l2@(_:ys)  =
  length l1 <= length l2 && (isPrefixOf l1 l2 || sublist' l1 ys)

superList :: (Eq a) => [a] -> [a] -> Bool
superList = flip sublist'

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
