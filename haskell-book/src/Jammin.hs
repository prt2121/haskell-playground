module Jammin where

data Fruit =
  Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

-- Rewrite JamJars with record syntax
-- Add Ord instances
data JamJars =
  Jam { fruit :: Fruit
      , jars :: Int }
      deriving (Eq, Show, Ord)

-- *Jammin> Jam Peach 5
-- Jam {fruit = Peach, jars = 5}

row1 = Jam Apple 7
row2 = Jam Peach 4
row3 = Jam Apple 9
row4 = Jam Plum 2
row5 = Jam Blackberry 3
allJam = [row1, row2, row3, row4, row5]

-- Write a function that will return the total number of jars of jam.
countJars :: [JamJars] -> Int
countJars = foldr ((+) . jars) 0
