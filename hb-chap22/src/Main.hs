module Main where

import Control.Applicative
import Control.Monad
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
  r <- rev
  c <- cap
  return (r, c)


newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
  name :: Name
  , dogName :: Name
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
  dogsName :: Name
  , dogsAddress :: Address
  } deriving (Eq, Show)

prat :: Person
prat =
  Person (Name "Prat")
  (Name "Snoopy")
  (Address "NJ")

getDogR' :: Person -> Dog
getDogR' =
  liftA2' Dog dogName address

liftA2' :: Applicative f
  => (a -> b -> c)
  -> f a -> f b -> f c
liftA2' f fa fb =
  f <$> fa <*> fb

-- asks :: (r -> a) -> Reader r a
-- asks f = Reader 

main :: IO ()
main = do
  putStrLn "hello world"
