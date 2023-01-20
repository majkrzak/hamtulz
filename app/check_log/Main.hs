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
  print (checkEmptyLoggingStation records)
  print (checkEmptyLoggingStationCallsign records)
  print (checkEmptyLoggingStationLocation records)
  mempty

checkOrder ::  [Log.Record] -> [String]
checkOrder [] = []
checkOrder [_] = []
checkOrder (Log.Record{datetime=d1}:x@Log.Record{datetime=d2}:xs) = (["log from " <> iso8601Show d2 <> " in wrong order" | d1>d2]) <> checkOrder (x:xs)

checkEmptyGrid :: [Log.Record] -> [String]
checkEmptyGrid [] = []
checkEmptyGrid (Log.Record{datetime=d, connection=Just Log.Connection {mode=Just Log.FT8}, stations=Just Log.Stations{contacted=Just Log.Station{location=Just Log.Location{gridsquare = Just ""}}}}:xs) = ["log from " <> iso8601Show d <> " have empty grid"] <> checkEmptyGrid xs
checkEmptyGrid (_:xs) = checkEmptyGrid xs

checkEmptyLoggingStation :: [Log.Record] -> [String]
checkEmptyLoggingStation [] = []
checkEmptyLoggingStation (Log.Record{datetime=d, stations=Just Log.Stations{logging=Nothing}}:xs) = ["log from " <> iso8601Show d <> " have empty logging station"] <> checkEmptyLoggingStation xs
checkEmptyLoggingStation (_:xs) = checkEmptyLoggingStation xs

checkEmptyLoggingStationCallsign :: [Log.Record] -> [String]
checkEmptyLoggingStationCallsign [] = []
checkEmptyLoggingStationCallsign (Log.Record{datetime=d, stations=Just Log.Stations{logging=Just Log.Station{callsign = Nothing}}}:xs) = ["log from " <> iso8601Show d <> " have empty logging station callsign"] <> checkEmptyLoggingStationCallsign xs
checkEmptyLoggingStationCallsign (_:xs) = checkEmptyLoggingStationCallsign xs

checkEmptyLoggingStationLocation :: [Log.Record] -> [String]
checkEmptyLoggingStationLocation [] = []
checkEmptyLoggingStationLocation (Log.Record{datetime=d, stations=Just Log.Stations{logging=Just Log.Station{location = Nothing}}}:xs) = ["log from " <> iso8601Show d <> " have empty logging station location"] <> checkEmptyLoggingStationLocation xs
checkEmptyLoggingStationLocation (_:xs) = checkEmptyLoggingStationLocation xs
