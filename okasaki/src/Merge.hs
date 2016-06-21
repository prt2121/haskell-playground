-- mod from https://gist.github.com/kazu-yamamoto/2987425
{-# LANGUAGE BangPatterns #-}

module Merge where

import Control.DeepSeq

newtype Schedule a = Schedule [[a]] deriving Show
data Segment a = Segment [a] (Schedule a) deriving Show
data MergeSort a = MergeSort Int [Segment a] deriving Show

instance NFData (Segment a) where rnf x = seq x ()

mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xs@(x:xs') ys@(y:ys')
  | x <= y    = x:mrg xs' ys
  | otherwise = y:mrg xs ys'

exec1 :: Schedule a -> Schedule a
exec1 (Schedule [])             = Schedule []
exec1 (Schedule ([]:sched))     = exec1 (Schedule sched)
exec1 (Schedule ((_:xs):sched)) = Schedule (xs:sched)

exec2 :: Segment a -> Segment a
exec2 (Segment xs sched) = Segment xs sched2
  where
    !sched1 = exec1 sched
    !sched2 = exec1 sched1

empty :: MergeSort a
empty = MergeSort 0 []

add :: Ord a => a -> MergeSort a -> MergeSort a
add x (MergeSort size segs) = MergeSort (size + 1) (map exec2 $!! segs')
  where
    segs' = addSeg [x] segs size []

addSeg :: Ord a => [a] -> [Segment a] -> Int -> [[a]] -> [Segment a]
addSeg xs segs size rsched
  | size `mod` 2 == 0 = Segment xs (Schedule (reverse rsched)) : segs
  | otherwise         = addSeg xs'' segs' (size `div` 2) (xs'':rsched)
  where
    (Segment xs' (Schedule [])) : segs' = segs
    xs'' = mrg xs xs'

sort :: Ord a => MergeSort a -> [a]
sort (MergeSort _ segs) = mrgAll [] segs
  where
    mrgAll xs [] = xs
    mrgAll xs (Segment xs' _ : segs') = mrgAll (mrg xs xs') segs'

infixl 0 >-
(>-) :: a -> (a -> b) -> b
a >- f = f a
