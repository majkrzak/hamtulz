module Main where
  
import Data.Log as Log
import qualified Data.Adif.Parser as AdifParser
import Data.Adif2Log (adif2log)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml.Builder (toByteString)
import Data.Yaml.Generic ()
import Data.Yaml.Instances ()
import Text.Parsec.String (parseFromFile)
import System.Environment.XDG.BaseDir (getUserDataFile)
import System.Environment (getArgs)
import Data.Yaml.AesonInstances ()
import Data.Yaml (decodeFileThrow)

main :: IO ()
main = do
  [logFile] <- getArgs
  adifFile <- getUserDataFile "WSJT-X" "wsjtx_log.adi"
  adif <- parseFromFile AdifParser.file adifFile
      >>= \case
            Left  err -> fail $ show err
            Right xs  -> return xs
  records :: [Log.Record] <- decodeFileThrow logFile
  let newRecords = filter (\Log.Record{datetime=d1} -> not $ any (\Log.Record{datetime=d2} -> d1 == d2) records) $ adif2log adif
  BS8.appendFile logFile $ toByteString newRecords
