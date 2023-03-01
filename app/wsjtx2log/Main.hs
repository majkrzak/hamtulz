module Main where

import Data.Adif.Parser qualified as AdifParser
import Data.Adif2Log (adif2log)
import Data.ByteString.Char8 qualified as BS8
import Data.Log as Log
import Data.Yaml (decodeFileThrow)
import Data.Yaml.AesonInstances ()
import Data.Yaml.Builder (toByteString)
import Data.Yaml.Generic ()
import Data.Yaml.Instances ()
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir (getUserDataFile)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  [logFile] <- getArgs
  adifFile <- getUserDataFile "WSJT-X" "wsjtx_log.adi"
  adif <-
    parseFromFile AdifParser.file adifFile
      >>= \case
        Left err -> fail $ show err
        Right xs -> return xs
  records :: [Log.Record] <- decodeFileThrow logFile
  let newRecords = filter (\Log.Record {datetime = d1} -> not $ any (\Log.Record {datetime = d2} -> d1 == d2) records) $ adif2log adif
  BS8.appendFile logFile $ toByteString newRecords
