module Main where

import Control.Lens (view, (^.))
import Data.Adi qualified as Adi
import Data.Adif qualified as Adif
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
  adiFile <- getUserDataFile "WSJT-X" "wsjtx_log.adi"
  adif <- Adif.fromAdi . read <$> readFile adiFile
  records :: [Log.Record] <- decodeFileThrow logFile
  let newRecords = filter (\rec1 -> not $ any (\rec2 -> (rec1 ^. Log.datetime) == (rec2 ^. Log.datetime)) records) $ Log.fromAdif (view Adif.records adif)
  BS8.appendFile logFile $ toByteString newRecords
