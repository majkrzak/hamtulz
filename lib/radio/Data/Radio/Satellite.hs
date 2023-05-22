module Data.Radio.Satellite
  ( Satellite (..),
  )
where

import Data.Empty (Empty)
import Data.Yaml.Builder (ToYaml, toYaml)
import Data.Yaml.Helper (showFromYaml, showToYaml)
import Data.Yaml.Parser (FromYaml, fromYaml)
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP (choice, string)
import Text.Read (lift, readEither, readPrec)

-- | Satellites enumeration
data Satellite
  = ARISS
  | SO50
  deriving (Eq, Generic, Empty)

instance Show Satellite where
  show ARISS = "ARISS"
  show SO50 = "SO-50"

instance Read Satellite where
  readPrec =
    lift $
      choice
        [ string "ARISS" >> return ARISS,
          string "SO-50" >> return SO50
        ]

instance FromYaml Satellite where
  fromYaml = showFromYaml

instance ToYaml Satellite where
  toYaml = showToYaml
