module Data.Empty where

import qualified Data.Log as Log
import Data.Time (UTCTime)

class Empty a where
  empty :: a

instance Empty UTCTime where
  empty = (read "2000-01-01 00:00:00.000000 UTC")

instance Empty (Maybe a) where
  empty = Nothing

instance Empty Log.Record where
  empty  = Log.Record
    { Log.datetime = empty
    , Log.stations = empty
    , Log.connection = empty
    , Log.report = empty
    }

instance Empty Log.Stations where
  empty = Log.Stations
    { Log.logging = empty
    , Log.contacted = empty
    }

instance Empty Log.Station where
  empty = Log.Station
    { Log.callsign = empty
    , Log.operator = empty
    , Log.location = empty
    , Log.antenna = empty
    , Log.power = empty
    }

instance Empty Log.Report where
  empty = Log.Report
    { Log.sent = Nothing
    , Log.rcvd = Nothing
    }

instance Empty Log.Connection where
  empty = Log.Connection
    { Log.band = Nothing
    , Log.mode = Nothing
    , Log.frequency = Nothing
    }
