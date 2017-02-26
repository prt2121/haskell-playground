{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

test :: IO ()
test = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction

-- Unit of Success

-- digit :: CharParsing m => m Char
-- eof :: Parsing m => m ()
-- integer :: TokenParsing m => m Integer

integer'' :: Parser Integer
integer'' = do
  i <- Text.Trifecta.integer
  eof
  return i

-- *Text.Fractions> parseString integer'' mempty "123"
-- Success 123
-- *Text.Fractions> parseString integer'' mempty "123abc"
-- Failure (ErrInfo {_errDoc = (interactive):1:4: error: expected: digit,
--     end of input
-- 123abc<EOF> 
--    ^        , _errDeltas = [Columns 3 3]})
