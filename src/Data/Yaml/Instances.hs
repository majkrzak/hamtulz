module Data.Yaml.Instances where

import           Data.Yaml.Builder              ( ToYaml
                                                , string
                                                , toYaml
                                                )


import           Data.Text                      ( pack )
import           Data.Time                      ( UTCTime )
import           Data.Time.Format.ISO8601       ( iso8601Show )



instance ToYaml UTCTime where
  toYaml = string . pack . iso8601Show
