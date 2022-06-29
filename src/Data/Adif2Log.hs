{-# LANGUAGE RecordWildCards #-}

module Data.Adif2Log (adif2log) where

import qualified Data.Log as Log
import qualified Data.Adif as Adif
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Data.Maybe (fromJust)


adif2log :: [Adif.Record] -> [Log.Record]
adif2log = map record2record
  where
    record2record :: Adif.Record -> Log.Record
    record2record Adif.Record {..} = Log.Record
      { Log.datetime = parseTimeOrError True defaultTimeLocale "%Y%m%d%H%M%S" (_qso_date <> _time_on) :: UTCTime
      , Log.stations = Just Log.Stations
        { Log.logging = Nothing
        , Log.contacted = Just Log.Station
          { Log.callsign = Just _call
          , Log.operator = Nothing
          , Log.location = Just Log.Location
            { Log.gridsquare = Just _gridsquare
            , Log.description = Nothing
            }
          , Log.antenna = Nothing
          , Log.power = Nothing
          }
        }
      , Log.connection = Just Log.Connection
        { Log.band = Just $ read _band
        , Log.mode = Just $ read _mode
        , Log.frequency = Just $ read _freq
        }
      , Log.report = Just Log.Report
        { Log.sent = Just _rst_sent
        , Log.rcvd = Just _rst_rcvd
        }
      }
