{-# OPTIONS_GHC -Wall #-}
-- homework 3
-- http://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
module Golf where

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex [] = []
zipWithIndex ls = zipWith (\i e -> (i, e)) [1..] ls

{-
The output of skips is a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the nth list in
the output should contain every nth element from the input list.
-}
skips :: [a] -> [[a]]
skips ls = map (map snd) (map filterByIndex (zipWithIndex(map zipWithIndex (replicate (length ls) ls))))
            where
                filterByIndex :: (Int, [(Int, b)]) -> [(Int, b)]
                filterByIndex p = filter (\x -> divisible (fst p) (fst x)) (snd p)
                divisible :: Int -> Int -> Bool
                divisible 1 _ = True
                divisible n m = m `mod` n == 0