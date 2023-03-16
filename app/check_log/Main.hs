module Main where

import Data.Log qualified as Log
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml (decodeFileWithWarnings)
import Data.Yaml.AesonInstances ()
import System.Environment (getArgs)

main :: IO ()
main = do
  [logFile] <- getArgs
  (warning, records :: [Log.Record]) <-
    decodeFileWithWarnings logFile
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

checkOrder :: [Log.Record] -> [String]
checkOrder [] = []
checkOrder [_] = []
checkOrder (Log.Record {Log.datetime = d1} : x@Log.Record {Log.datetime = d2} : xs) = (["log from " <> iso8601Show d2 <> " in wrong order" | d1 > d2]) <> checkOrder (x : xs)

checkEmptyGrid :: [Log.Record] -> [String]
checkEmptyGrid [] = []
checkEmptyGrid (Log.Record {Log.datetime = d, Log.connection = Just Log.Connection {Log.mode = Just Log.FT8}, Log.stations = Just Log.Stations {Log.contacted = Just Log.Station {Log.location = Just Log.Location {Log.gridsquare = Just ""}}}} : xs) = ["log from " <> iso8601Show d <> " have empty grid"] <> checkEmptyGrid xs
checkEmptyGrid (_ : xs) = checkEmptyGrid xs

checkEmptyLoggingStation :: [Log.Record] -> [String]
checkEmptyLoggingStation [] = []
checkEmptyLoggingStation (Log.Record {Log.datetime = d, Log.stations = Just Log.Stations {Log.logging = Nothing}} : xs) = ["log from " <> iso8601Show d <> " have empty logging station"] <> checkEmptyLoggingStation xs
checkEmptyLoggingStation (_ : xs) = checkEmptyLoggingStation xs

checkEmptyLoggingStationCallsign :: [Log.Record] -> [String]
checkEmptyLoggingStationCallsign [] = []
checkEmptyLoggingStationCallsign (Log.Record {Log.datetime = d, Log.stations = Just Log.Stations {Log.logging = Just Log.Station {Log.callsign = Nothing}}} : xs) = ["log from " <> iso8601Show d <> " have empty logging station callsign"] <> checkEmptyLoggingStationCallsign xs
checkEmptyLoggingStationCallsign (_ : xs) = checkEmptyLoggingStationCallsign xs

checkEmptyLoggingStationLocation :: [Log.Record] -> [String]
checkEmptyLoggingStationLocation [] = []
checkEmptyLoggingStationLocation (Log.Record {Log.datetime = d, Log.stations = Just Log.Stations {Log.logging = Just Log.Station {Log.location = Nothing}}} : xs) = ["log from " <> iso8601Show d <> " have empty logging station location"] <> checkEmptyLoggingStationLocation xs
checkEmptyLoggingStationLocation (_ : xs) = checkEmptyLoggingStationLocation xs
