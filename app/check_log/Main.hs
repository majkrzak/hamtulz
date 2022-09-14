module Main where

import Data.Log as Log
import Data.Yaml.AesonInstances ()
import Data.Yaml (decodeFileWithWarnings)
import System.Environment (getArgs)
import Data.Time.Format.ISO8601 (iso8601Show)

main :: IO ()
main = do
  [logFile] <- getArgs
  (warning, records:: [Log.Record]) <- decodeFileWithWarnings logFile
    >>= \case
      Left err -> fail $ show err
      Right xs -> return xs
  print warning
  print (checkOrder records)
  print (checkEmptyGrid records)
  mempty

checkOrder ::  [Log.Record] -> [String]
checkOrder [] = []
checkOrder [_] = []
checkOrder (Log.Record{datetime=d1}:x@Log.Record{datetime=d2}:xs) = (["log from " <> iso8601Show d2 <> " in wrong order" | d1>d2]) <> checkOrder (x:xs)

checkEmptyGrid :: [Log.Record] -> [String]
checkEmptyGrid [] = []
checkEmptyGrid (Log.Record{datetime=d, connection=Just Log.Connection {mode=Just Log.FT8}, stations=Just Log.Stations{contacted=Just Log.Station{location=Just Log.Location{gridsquare = Just ""}}}}:xs) = ["log from " <> iso8601Show d <> " have empty grid"] <> checkEmptyGrid xs
checkEmptyGrid (_:xs) = checkEmptyGrid xs
