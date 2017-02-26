module Main where

import Text.Trifecta
import Control.Applicative
import Control.Monad

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

-- parseString
--   :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a
-- parseString one mempty "1"
-- Success '1'

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

-- make the one and oneTwo parsers fail bc they didn't exhaust the input stream

one'' :: Parser ()
one'' = one >> eof

oneTwo'' :: Parser ()
oneTwo'' = oneTwo >> eof

oneStr :: Parser String
oneStr = string "1"

twoStr :: Parser String
twoStr = string "2"

threeStr :: Parser String
threeStr = string "3"

oneTwoThree :: Parser String
oneTwoThree = string "123"

alter :: Parser String
alter = string "123" <|> string "12" <|> string "1"

-- Try writing a Parser that does what string does, but using char.

string' :: String -> Parser String
string' str = foldM (\ls c -> (ls ++) . (:[]) <$> char c) "" str

-- parseString
--   :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a

pNL s =
  putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
