{-# LANGUAGE OverloadedStrings #-}
-- https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
module Main where

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

data LoginError = InvalidEmail
  deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

main :: IO ()
main = do
  putStrLn "hello world"

-- *Main> :set -XOverloadedStrings
-- *Main> getDomain "test@example.com"
-- Right "example.com"
