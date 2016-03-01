module Strain where

keep :: (a -> Bool) -> [a] -> [a]
keep p ls = foldr (\x acc -> if p x then x : acc else acc) [] ls

discard :: (a -> Bool) -> [a] -> [a]
discard = filter . (not .)
