{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , app
    ) where

import           Api
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Foldable
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           Debug.Trace
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment


startApp :: IO ()
startApp = do
  env <- getEnvironment
  let maybeConnectInfo =
        mkConnectInfo <$> (lookup "DB_NAME" env)
                      <*> (lookup "DB_USER" env)
                      <*> (lookup "DB_PASSWORD" env)
  maybeApp <- sequence $ mkApp <$> maybeConnectInfo
  sequence_ $ run 8080 <$> maybeApp

mkConnectInfo :: String -> String -> String -> ConnectInfo
mkConnectInfo db user password = defaultConnectInfo {
    connectPort = 3306,
    connectUser = user,
    connectPassword = password,
    connectDatabase = db
  }

app :: ConnectionPool -> Application
app pool = serve api $ server pool

server :: ConnectionPool -> Server API
server pool = usersGetH :<|> usersPostH where
  usersGetH = trace "get" liftIO usersGet
  usersPostH = trace "post" liftIO . usersPost

  usersGet :: IO [User]
  usersGet = flip runSqlPersistMPool pool $ do
    users <- selectList [] []
    return $ entityVal <$> users

  usersPost :: User -> IO (Key User)
  usersPost newUser = flip runSqlPersistMPool pool $ insert newUser

mkApp :: ConnectInfo -> IO Application
mkApp connectInfo = runStderrLoggingT $ withMySQLPool connectInfo 10 $ \pool -> liftIO $ return $ app pool
