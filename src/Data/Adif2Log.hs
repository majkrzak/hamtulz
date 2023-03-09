module Data.Adif2Log (adif2log) where

import Data.Adif qualified as Adif
import Data.Log qualified as Log
import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Data.Empty (empty)
import Control.Lens ((^.))

adif2log :: [Adif.Record] -> [Log.Record]
adif2log = map record2record
  where
    record2record :: Adif.Record -> Log.Record
    record2record r =
      Log.Record
        { Log.datetime = parseTimeOrError True defaultTimeLocale "%Y%m%d%H%M%S" (fromJust (r ^. Adif._qso_date) <> fromJust (r ^. Adif._time_on)) :: UTCTime,
          Log.stations =
            Just
              Log.Stations
                { Log.logging = Nothing,
                  Log.contacted =
                    Just
                      Log.Station
                        { Log.callsign = r ^. Adif._call,
                          Log.operator = Nothing,
                          Log.location =
                            Just
                              Log.Location
                                { Log.gridsquare = r ^. Adif._gridsquare,
                                  Log.description = Nothing,
                                  Log.program = Nothing
                                },
                          Log.antenna = Nothing,
                          Log.power = Nothing
                        }
                },
          Log.connection =
            Just
              Log.Connection
                { Log.band = read <$> r ^. Adif._band,
                  Log.mode = read <$> r ^. Adif._mode,
                  Log.frequency = read <$> r ^. Adif._freq
                },
          Log.report =
            Just
              Log.Report
                { Log.sent = r ^.Adif._rst_sent,
                  Log.rcvd = r ^. Adif._rst_rcvd
                }
        }
