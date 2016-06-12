{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B

data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                  deriving (Eq, Ord, Show)

instance Error ParseError where
    noMsg  = Chatty "oh noes!"
    strMsg = Chatty

newtype Parser a = P {
                      runP :: ErrorT ParseError (State B.ByteString) a
                   } deriving (Monad, MonadError ParseError)
