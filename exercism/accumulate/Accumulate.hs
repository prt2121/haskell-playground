module Accumulate where
-- http://exercism.io/exercises/haskell/accumulate
accumulate :: (a -> b) -> [a] -> [b]
accumulate f = foldr ((:) . f) []
