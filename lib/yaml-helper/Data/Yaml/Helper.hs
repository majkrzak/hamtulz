module Data.Yaml.Helper (showFromYaml, showToYaml) where

import Data.Text (pack, unpack)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Data.Yaml.Builder (YamlBuilder, string)
import Data.Yaml.Parser (YamlParser, YamlValue, withText)
import Text.Read (readEither)

showFromYaml :: Read a => YamlValue -> YamlParser a
showFromYaml = withText mempty $ either fail return . readEither . unpack

showToYaml :: Show a => a -> YamlBuilder
showToYaml = string . pack . show
