{-# LANGUAGE OverloadedStrings #-}
-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes #-}

module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, _Array, key)

main :: IO()
main = do
  r <- get "https://api.github.com/users/prt2121/repos"
  putStrLn $ show $ r ^.. responseBody . _Array . traverse . key "git_url" . _String
--  putStrLn $ show $ r ^? responseBody . _Array . traverse . key "git_url" . _String

-- ^?
-- Perform a safe head of a Fold or Traversal or retrieve Just the result from a Getter or Lens.
-- ^..
-- (^..) :: s -> Getting (Endo [a]) s a -> [a]
-- A convenient infix (flipped) version of toListOf

-- toListOf :: Getting (Endo [a]) s a -> s -> [a] Source
--
-- Extract a list of the targets of a Fold. See also (^..).
--
-- toList ≡ toListOf folded
-- (^..) ≡ flip toListOf

-- Traversals are Lenses which focus on multiple targets simultaneously
-- Operators that begin with ^ are kinds of views.

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
