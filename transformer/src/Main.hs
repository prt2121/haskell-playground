{-# LANGUAGE OverloadedStrings #-}
-- https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
module Main where

import           Control.Applicative
import           Data.Map            as Map
import           Data.Text
import qualified Data.Text.IO        as T

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show

data EitherIO e a = EitherIO {
                      runEitherIO :: IO (Either e a)
                    }

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

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

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)

getDomainIO :: EitherIO LoginError Text
getDomainIO = do
  liftIO (T.putStrLn "Enter email address:")
  input <- liftIO T.getLine
  liftEither (getDomain input)

users :: Map Text Text
users = Map.fromList [("test.com", "test1234"), ("localhost", "password")]

userLogin :: EitherIO LoginError Text
userLogin = do
  d <- getDomainIO
  p <- maybe
        (liftEither $ Left NoSuchUser)
        return $ Map.lookup d users
  password <- liftIO (T.putStrLn "Enter your password:" >> T.getLine)
  if p == password
     then return d
     else throwE WrongPassword

throwE :: e -> EitherIO e a
throwE e = liftEither $ Left e

main :: IO ()
main = do
  T.putStrLn "hello world"

-- *Main> getDomainIO
-- Enter email address:
-- b@gmail.com
-- Right "gmail.com"

-- runEitherIO :: EitherIO e a -> IO (Either e a)
-- fmap :: (a -> b) -> f a -> f b

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- *Main> :set -XOverloadedStrings
-- *Main> getDomain "test@example.com"
-- Right "example.com"
-- *Main> printResult $ getDomain "test@example.com"
-- Domain: example.com
