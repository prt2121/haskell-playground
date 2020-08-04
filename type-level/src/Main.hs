module Main where

import Lib

earthRadius :: Len KM
earthRadius = 6371

challengerDeep :: Len MI
challengerDeep = 6.8

nonsense :: Len Bool
nonsense = 0

main :: IO ()
main = do
  putStrLn "hello world"
