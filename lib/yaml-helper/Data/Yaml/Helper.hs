module Data.Yaml.Helper (showFromYaml, showToYaml, parseYaml) where

import Data.Conduit (runConduitRes, yield, (.|))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Data.Yaml.Builder (YamlBuilder, string)
import Data.Yaml.Parser (FromYaml, YamlParser, YamlValue, fromYaml, parseRawDoc, sinkRawDoc, withText)
import System.IO.Unsafe (unsafePerformIO)
import Text.Libyaml (decode)
import Text.Read (readMaybe)

showFromYaml :: forall a. (Read a, Typeable a) => YamlValue -> YamlParser a
showFromYaml = withText (pack typename) $ \x ->
  maybe (fail $ "Can not parse " ++ show x ++ " as " ++ typename) return $ readMaybe $ unpack x
  where
    typename :: String = show $ typeRep (Proxy :: Proxy a)

showToYaml :: Show a => a -> YamlBuilder
showToYaml = string . pack . show

instance FromYaml UTCTime where
  fromYaml = withText "UTCTime" (iso8601ParseM . unpack)

parseYaml :: FromYaml a => Text -> Maybe a
parseYaml s = do
  parseRawDoc $ unsafePerformIO $ runConduitRes (decode (encodeUtf8 s) .| sinkRawDoc)
