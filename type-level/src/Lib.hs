{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Lib where

import Data.Proxy

newtype Len unit = Len Double
  deriving (Num, Fractional)

data MI
data KM

miToKm :: Len MI -> Len KM
miToKm (Len mi) = Len $ mi * 1.60934

kmToMi :: Len KM -> Len MI
kmToMi (Len km) = Len $ km / 1.60934

class UnitName u where
  unitName :: Proxy u -> String

instance UnitName MI where
  unitName :: Proxy MI -> String
  unitName _ = "mi"

instance UnitName KM where
  unitName :: Proxy KM -> String
  unitName _ = "km"

instance UnitName unit => UnitName (Len unit) where
  unitName _ = unitName (Proxy :: Proxy unit)

instance UnitName unit => Show (Len unit) where
  show (Len l) = show l ++ " " ++unitName (Proxy :: Proxy unit)

instance UnitName Len where
  unitName _ = "_unspecified unit_"
