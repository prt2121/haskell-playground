module Main where

import Prelude

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

-- instance Eq Trivial where
--   (==) Trivial' Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  Mon == Mon = True
  Tue == Tue = True
  Weds == Weds = True
  Thu == Thu = True
  Fri == Fri = True
  Sat == Sat = True
  Sun == Sun = True
  _ == _ = False

instance Show DayOfWeek where
  show Mon = "Mon"
  show Tue = "Tue"
  show Weds = "Weds"
  show Thu = "Thu"
  show Fri = "Fri"
  show Sat = "Sat"
  show Sun = "Sun"

instance Eq Date where
  (Date d i) == (Date d' i') = d == d' && i == i'

instance Show Date where
  show (Date d i) = show d ++ " " ++ show i

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (TisAn i) == (TisAn i') = i == i'

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two i j) (Two i' j') = (i == i') && (j == j')

data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i) (TisAnInt j) = i == j
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair f s) (Pair f' s') = f == f' && s == s'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

data Which a =
  ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

data EitherOr a b =
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False

class (Num a) => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

-- 6.14 Chapter Exercises
-- The Eq class
-- makes equality tests possible

-- Ord
-- is a subclass of Eq

-- Suppose the typeclass Ord has an operator >. What is the type of >?
-- Ord a => a -> a -> Bool

data Person = Person Bool deriving Show
data Mood = Blah | Woot deriving (Eq, Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

settleDown x = if x == Woot then Blah else x

x :: (Integer, Integer)
x = divMod 16 12

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk ab a b = (ab a) == b

-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
-- wtf
arith :: Num b => (a -> b) -> Integer -> a -> b
arith ab i a = (ab a) + fromInteger i
-- arith ab i a = ab a

data Rocks =
  Rocks String deriving (Eq, Ord, Show)
data Yeah =
  Yeah Bool deriving (Eq, Ord, Show)
data Papu =
  Papu Rocks Yeah deriving (Eq, Ord, Show)

x' :: Int -> Int
x' blah = blah + 20

-- x'
-- printIt :: IO ()
-- printIt = putStrLn (show x')

phew = Papu (Rocks "chases") $ Yeah True

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

main :: IO ()
main = do
  putStrLn "hello world"
