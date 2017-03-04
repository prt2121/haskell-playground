module Main where

import Data.Monoid
import Test.QuickCheck

-- *Main> :t mappend
-- mappend :: Monoid a => a -> a -> a
-- *Main> :t mconcat
-- mconcat :: Monoid a => [a] -> a

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    Nada `mappend` m = m
    m `mappend` Nada = m
    Only m1 `mappend` Only m2 = Only (m1 `mappend` m2)

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbin'' :: Exclamation
            -> Adverb
            -> Noun
            -> Adjective
            -> String
madlibbin'' e adv noun adj =
  mconcat [e , "! he said " ,
  adv , " as he jumped into his car " ,
  noun , " and drove off with his " ,
  adj , " wife."]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> b) <> c == a <> (b <> c)

monoidLeftId :: (Eq m, Monoid m) => m -> Bool
monoidLeftId a = mempty <> a == a
-- *Main> quickCheck (monoidLeftId :: String -> Bool)
-- +++ OK, passed 100 tests.

monoidRightId :: (Eq m, Monoid m) => m -> Bool
monoidRightId a = a <> mempty == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty
    = First' { getFirst' = Nada }
  mappend First' { getFirst' = Nada } x
    = x
  mappend First' { getFirst' = Only x } First' { getFirst' = Nada }
    = First' { getFirst' = Only x}
  mappend First' { getFirst' = Only x } First' { getFirst' = Only _ }
    = First' { getFirst' = Only x }

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
  First' String
    -> First' String
    -> First' String
    -> Bool
type FstId =
  First' String -> Bool

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  x <- arbitrary
  return $ Only x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [ (1, genOnly)
              , (1, return Nada) ]

genFirst :: Arbitrary a => Gen (First' a)
genFirst =
  do
    x <- arbitrary
    return First' { getFirst' = x }

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftId :: FstId)
  quickCheck (monoidRightId :: FstId)

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
