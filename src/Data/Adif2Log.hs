module Data.Adif2Log (adif2log) where

import qualified Data.Log as Log
import qualified Data.Adif as Adif
import Data.List (find)
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Data.Maybe (fromJust)


adif2log :: Adif.File -> [Log.Record]
adif2log (Adif.File (_, records)) = map record2record records
  where
    record2record :: Adif.Record -> Log.Record
    record2record (Adif.Record fields) = Log.Record
      { Log.datetime = parseTimeOrError True defaultTimeLocale "%Y%m%d%H%M%S" (fromJust (findField "qso_date") <> fromJust (findField "time_on")) :: UTCTime
      , Log.stations = Just Log.Stations
        { Log.logging = Nothing
        , Log.contacted = Just Log.Station
          { Log.callsign = findField "call"
          , Log.operator = Nothing
          , Log.location = Just Log.Location
            { Log.gridsquare = findField "gridsquare"
            , Log.description = Nothing
            }
          , Log.antenna = Nothing
          , Log.power = Nothing
          }
        }
      , Log.mode = read <$> findField "mode"
      , Log.frequency = read <$> findField "freq"
      , Log.report = Just Log.Report
        { Log.sent = findField "rst_sent"
        , Log.rcvd = findField "rst_rcvd"
        }
      }
      where
        findField :: String -> Maybe String
        findField key = do
         Adif.Field (_, val) <- find (\(Adif.Field(key',_)) -> key == key') fields
         return val
