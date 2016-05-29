module Main where

import Data.Monoid

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    Nada `mappend` m = m
    m `mappend` Nada = m
    Only m1 `mappend` Only m2 = Only (m1 `mappend` m2)

main :: IO ()
main = do
  putStrLn "chap 15"

-- *Main> Only (Sum 1) `mappend` Only (Sum 1)
-- Only (Sum {getSum = 2})
-- *Main> Only (Sum 1) <>Only (Sum 1)
-- Only (Sum {getSum = 2})
-- *Main> Only (Sum 1) <> Only (Sum 1)
-- Only (Sum {getSum = 2})
-- *Main> Only (Product 4) `mappend` Only (Product 2)
-- Only (Product {getProduct = 8})
-- *Main> Only (Sum 1) `mappend` Nada
-- Only (Sum {getSum = 1})
-- *Main> Only [1] `mappend` Nada
-- Only [1]
-- *Main> Nada `mappend` Only (Sum 1)
-- Only (Sum {getSum = 1})
