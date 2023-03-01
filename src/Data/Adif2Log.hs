module Data.Adif2Log (adif2log) where

import Data.Adif qualified as Adif
import Data.Log qualified as Log
import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)

adif2log :: [Adif.Record] -> [Log.Record]
adif2log = map record2record
  where
    record2record :: Adif.Record -> Log.Record
    record2record r =
      Log.Record
        { Log.datetime = parseTimeOrError True defaultTimeLocale "%Y%m%d%H%M%S" (fromJust (Adif._qso_date r) <> fromJust (Adif._time_on r)) :: UTCTime,
          Log.stations =
            Just
              Log.Stations
                { Log.logging = Nothing,
                  Log.contacted =
                    Just
                      Log.Station
                        { Log.callsign = Adif._call r,
                          Log.operator = Nothing,
                          Log.location =
                            Just
                              Log.Location
                                { Log.gridsquare = Adif._gridsquare r,
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
                { Log.band = read <$> Adif._band r,
                  Log.mode = read <$> Adif._mode r,
                  Log.frequency = read <$> Adif._freq r
                },
          Log.report =
            Just
              Log.Report
                { Log.sent = Adif._rst_sent r,
                  Log.rcvd = Adif._rst_rcvd r
                }
        }
