{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Models where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           Database.Persist.TH
import           Servant

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  firstName String
  lastName String
  deriving Eq Read Show
|]

$(deriveJSON defaultOptions ''User)
