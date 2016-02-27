module Strain where

keep :: (a -> Bool) -> [a] -> [a]
keep = filter

discard :: (a -> Bool) -> [a] -> [a]
discard = filter . (not .)
