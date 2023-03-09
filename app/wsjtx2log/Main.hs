module Main where

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
import Data.Adi qualified as Adi
import Data.Adif qualified as Adif
import Control.Lens (view)

main :: IO ()
main = do
  [logFile] <- getArgs
  adiFile <- getUserDataFile "WSJT-X" "wsjtx_log.adi"
  adif <- Adif.fromAdi . read <$> readFile adiFile
  records :: [Log.Record] <- decodeFileThrow logFile
  let newRecords = filter (\Log.Record {datetime = d1} -> not $ any (\Log.Record {datetime = d2} -> d1 == d2) records) $ adif2log (view Adif.records adif)
  BS8.appendFile logFile $ toByteString newRecords
