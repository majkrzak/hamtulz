module Main where

import Data.Log as Log
import Data.Log2Adif(log2adif)
import Data.Yaml.AesonInstances ()
import Data.Yaml (decodeFileThrow)
import Data.Adif.Builder (toAdif)
import System.Environment (getArgs)

main :: IO ()
main = do
  [logFile, adifFile] <- getArgs
  records :: [Log.Record] <- decodeFileThrow logFile
  writeFile adifFile (toAdif $ log2adif records)
