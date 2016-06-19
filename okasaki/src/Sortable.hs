module Sortable (Sortable(..)) where

class Sortable s where
  new :: Ord a => s a
  add   :: Ord a => a -> s a -> s a
  sort  :: Ord a => s a -> [a]
