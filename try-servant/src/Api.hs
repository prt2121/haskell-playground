{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text
import Database.Persist
import Models
import Servant

type API = "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Key User)

api :: Proxy API
api = Proxy
