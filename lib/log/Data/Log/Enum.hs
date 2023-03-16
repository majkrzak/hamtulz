module Data.Log.Enum where

import Data.Empty (Empty)
import GHC.Generics (Generic)

data Band = Two | Six | Ten | Twelve | Fifteen | Seventeen | Twenty | Thirty | Forty | Eighty deriving (Eq, Generic, Empty)

instance Show Band where
  show Two = "2m"
  show Six = "6m"
  show Ten = "10m"
  show Twelve = "12m"
  show Fifteen = "15m"
  show Seventeen = "17m"
  show Twenty = "20m"
  show Thirty = "30m"
  show Forty = "40m"
  show Eighty = "80m"

instance Read Band where
  readsPrec _ "2m" = [(Two, "")]
  readsPrec _ "6m" = [(Six, "")]
  readsPrec _ "10m" = [(Ten, "")]
  readsPrec _ "12m" = [(Twelve, "")]
  readsPrec _ "15m" = [(Fifteen, "")]
  readsPrec _ "17m" = [(Seventeen, "")]
  readsPrec _ "20m" = [(Twenty, "")]
  readsPrec _ "30m" = [(Thirty, "")]
  readsPrec _ "40m" = [(Forty, "")]
  readsPrec _ "80m" = [(Eighty, "")]
  readsPrec _ _ = []

data Mode = CW | SSB | FM | FT8 deriving (Eq, Show, Read, Generic, Empty)
