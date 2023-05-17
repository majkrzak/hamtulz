module Data.Radio.Mode
  ( Mode (..),
  )
where

import Data.Empty (Empty)
import Data.Yaml.Builder (ToYaml, toYaml)
import Data.Yaml.Helper (showFromYaml, showToYaml)
import Data.Yaml.Parser (FromYaml, fromYaml)
import GHC.Generics (Generic)

-- | Radio Modes enumeration
-- Based on but not compliant with https://adif.org/314/ADIF_314.htm#Mode_Enumeration
data Mode = CW | SSB | FM | FT8 | FT4 | DMR deriving (Eq, Show, Read, Generic, Empty)

instance FromYaml Mode where
  fromYaml = showFromYaml

instance ToYaml Mode where
  toYaml = showToYaml
