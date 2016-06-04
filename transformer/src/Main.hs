{-# LANGUAGE OverloadedStrings #-}
-- https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
module Main where

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

printResult :: Either LoginError Text -> IO ()
printResult domain =
  case domain of
    Right text        -> T.putStrLn (append "Domain: " text)
    Left InvalidEmail -> T.putStrLn "ERROR: Invalid domain"

-- *Main> getDomainIO
-- Enter email address:
-- b@gmail.com
-- Right "gmail.com"

getDomainIO :: IO (Either LoginError Text)
getDomainIO = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  return (getDomain email)

users :: Map Text Text
users = Map.fromList [("test.com", "test1234"), ("localhost", "password")]

userLogin :: IO (Either LoginError Text)
userLogin = do
  d <- getDomainIO
  case d of
    Right domain ->
      case Map.lookup domain users of
        Just p -> do
          T.putStrLn "Enter password:"
          password <- T.getLine
          if p == password
             then return d
             else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left

main :: IO ()
main = do
  T.putStrLn "hello world"

-- *Main> :set -XOverloadedStrings
-- *Main> getDomain "test@example.com"
-- Right "example.com"
-- *Main> printResult $ getDomain "test@example.com"
-- Domain: example.com
