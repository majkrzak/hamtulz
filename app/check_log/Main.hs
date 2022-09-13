module Main where

import Data.Log as Log
import Data.Yaml.AesonInstances ()
import Data.Yaml (decodeFileWithWarnings)
import System.Environment (getArgs)

main :: IO ()
main = do
  [logFile] <- getArgs
  (warning, records:: [Log.Record]) <- decodeFileWithWarnings logFile
    >>= \case
      Left err -> fail $ show err
      Right xs -> return xs
  print warning
  print (checkOrder records)
  mempty

checkOrder ::  [Log.Record] -> [String]
checkOrder [] = []
checkOrder [_] = []
checkOrder (Log.Record{datetime=d1}:x@Log.Record{datetime=d2}:xs) = (["log from " <> show d2 <> " in wrong order" | d1>d2]) <> checkOrder (x:xs)
