{-# LANGUAGE InstanceSigs #-}

module Chapter25 where

import Control.Monad.Identity
import Control.Applicative
import Data.Functor

-- exercise: CHAPTER 25. E PLURIBUS MONAD
instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose (liftA2 (<*>) f a)
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

instance (Foldable f, Foldable g) =>
  Foldable (Compose f g) where
  -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldMap f (Compose a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
  -- Map each element of a structure to an action, evaluate these actions from left to right, and collect the results
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Compose t) = Compose <$> traverse (traverse f) t

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

newtype One f a =
  One (f a)
  deriving (Eq, Show)

newtype Three f g h a =
  Three (f (g (h a)))

instance Functor f =>
  Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

instance (Functor f, Functor g, Functor h) =>
  Functor (Three f g h) where
  fmap f (Three fgha) =
    Three $ (fmap . fmap . fmap) f fgha

-- instance Functor Identity where
--   fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fmap f (Compose fga) =
      Compose $ (fmap . fmap) f fga

-- *Chapter25> Compose [Just "a", Nothing]
-- Compose {getCompose = [Just "a",Nothing]}
-- *Chapter25> Compose [Just "a", Nothing, Just "123"]
-- Compose {getCompose = [Just "a",Nothing,Just "123"]}

-- *Chapter25> :t Compose
-- Compose :: f (g a) -> Compose f g a
-- *Chapter25> :t getCompose
-- getCompose :: Compose f g a -> f (g a)
-- *Chapter25> :k Compose
-- Compose :: (* -> *) -> (* -> *) -> * -> *
