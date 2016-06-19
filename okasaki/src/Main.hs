module Main where

import Sortable

data MergeSort a = MergeSort Int [[a]]
                    deriving Show
-- [[a]] ~ Susp List List a

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xs') ys@(y:ys') =
  if x <= y then x : merge xs' ys else y : merge xs ys'

-- add :: (Ord a, Sortable s) => a -> s a -> s a
-- addSeg :: (Integral a, Ord b) => [b] -> [[b]] -> a -> [[b]]
-- sort :: (Ord a, Sortable s) => s a -> [a]

instance Sortable MergeSort where
  new = MergeSort 0 []

  -- add takes O(log n) amortized time
  add x (MergeSort size segs) =
    let addSeg seg segs size =
          if size `mod` 2 == 0 then seg : segs
          else addSeg (merge seg (head segs)) (tail segs) (size `div` 2)
    in MergeSort (size+1) (addSeg [x] segs size) -- $addSeg

  -- sort takes O(n)  amortized time
  sort (MergeSort size segs) = foldl merge [] segs

main :: IO ()
main = do putStrLn "MergeSort"
