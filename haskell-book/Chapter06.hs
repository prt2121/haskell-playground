module Chapter06 where

-- CHAPTER 6. TYPECLASSES p. 206

data TisAnInteger =
  TisAn Integer deriving (Show)

data TwoIntegers =
  Two Integer Integer deriving (Show)

data StringOrInt =
  TisAnInt Int
  | TisAString String deriving (Show)

data Pair a =
  Pair a a deriving (Show)

data Tuple a b =
  Tuple a b

data Which a =
  ThisOne a
  | ThatOne a deriving (Show)

data EitherOr a b =
  Hello a
  | Goodbye b

instance Eq TisAnInteger where
  (==) (TisAn z) (TisAn z') =
    z == z'

instance Eq TwoIntegers where
  (==) (Two m n) (Two m' n') =
    m == m' && n == n'

-- *Chapter06> Two 1 2 == Two 1 2
-- True

instance Eq StringOrInt where
  (==) (TisAnInt z) (TisAnInt z') = z == z'
  (==) (TisAString z) (TisAString z') = z == z'

-- *Chapter06> TisAString "xyz" == TisAString "xyz"
-- True
-- *Chapter06> TisAString "xyz" == TisAString "xyzz"
-- False

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

-- *Chapter06> (Pair 1 2) == (Pair 1 2)
-- True

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-- *Chapter06> (Tuple 1 'a') == (Tuple 1 'a')
-- True
-- *Chapter06> (Tuple 1 'a') == (Tuple 2 'a')
-- False

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
