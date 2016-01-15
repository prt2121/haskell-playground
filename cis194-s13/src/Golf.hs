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

-- pointfree version
-- skips' = map (map snd) . map filterByIndex . zipWithIndex . map zipWithIndex . (replicate =<< length)

-- Exercise 2 Local maxima
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it.
--     localMaxima [2,9,5,6,1] == [9,6]
--     localMaxima [2,3,4,1,5] == [4]
--     localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima = concat . map middleMax . group3
                where
                    middleMax :: [Integer] -> [Integer]
                    middleMax (a:b:[c]) = if b > a && b > c then [b] else []
                    middleMax _         = []
                    group3 :: [t] -> [[t]]
                    group3 ls = if length ls < 3
                                then []
                                else take 3 ls : group3 (tail ls)