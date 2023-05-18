module Data.Yaml.Helper (showFromYaml, showToYaml, parseYaml) where

import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Data.Yaml.Builder (YamlBuilder, string)
import Data.Yaml.Parser (FromYaml, YamlParser, YamlValue, fromYaml, withText, sinkRawDoc, parseRawDoc)
import Text.Read (readEither)
import Text.Libyaml (decode)
import Data.Conduit (runConduitRes, yield, (.|))
import System.IO.Unsafe (unsafePerformIO)

showFromYaml :: Read a => YamlValue -> YamlParser a
showFromYaml = withText mempty $ either fail return . readEither . unpack

showToYaml :: Show a => a -> YamlBuilder
showToYaml = string . pack . show

instance FromYaml UTCTime where
  fromYaml = withText "UTCTime" (iso8601ParseM . unpack)

parseYaml :: FromYaml a => Text -> Maybe a
parseYaml s = do
    let raw = unsafePerformIO $ runConduitRes (decode (encodeUtf8 s) .| sinkRawDoc)
    res <- parseRawDoc raw
    return res
