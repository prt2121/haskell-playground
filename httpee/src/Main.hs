{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, _Array)

main :: IO()
main = do
  r <- get "https://api.github.com/users/prt2121/repos"
  putStrLn $ show $ r ^? responseBody
