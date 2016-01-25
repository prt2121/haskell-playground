-- my fav solution (not using isInfixOf) by wricardo
module Sublist ( Sublist(Equal, Sublist, Superlist, Unequal), sublist) where

data Sublist =  Equal | Sublist | Superlist | Unequal deriving (Show, Eq)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist a b
  | a  == b = Equal
  | sub a b  == True = Sublist
  | super a b == True = Superlist
  | otherwise = Unequal

super :: (Eq a) => [a] -> [a] -> Bool
super a b = flip sub a b

sub :: (Eq a) => [a] -> [a] -> Bool
sub  a b
  | length a >  length b = False
  | take (length a) b == a = True
  | otherwise = sub a (tail b)
