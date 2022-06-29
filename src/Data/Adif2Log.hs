module Data.Adif2Log
  ( adif2log
  ) where

import qualified Data.AdifOld                  as Adif
import           Data.List                      ( find )
import qualified Data.Log                      as Log
import           Data.Maybe                     ( fromJust )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , parseTimeOrError
                                                )


adif2log :: Adif.File -> [Log.Record]
adif2log (Adif.File (_, records)) = map record2record records
 where
  record2record :: Adif.Record -> Log.Record
  record2record (Adif.Record fields) = Log.Record
    { Log.datetime   = parseTimeOrError
                         True
                         defaultTimeLocale
                         "%Y%m%d%H%M%S"
                         (  fromJust (findField "qso_date")
                         <> fromJust (findField "time_on")
                         ) :: UTCTime
    , Log.stations   = Just Log.Stations
      { Log.logging   = Nothing
      , Log.contacted = Just Log.Station
        { Log.callsign = findField "call"
        , Log.operator = Nothing
        , Log.location = Just Log.Location
                           { Log.gridsquare  = findField "gridsquare"
                           , Log.description = Nothing
                           }
        , Log.antenna  = Nothing
        , Log.power    = Nothing
        }
      }
    , Log.connection = Just Log.Connection
                         { Log.band      = read <$> findField "band"
                         , Log.mode      = read <$> findField "mode"
                         , Log.frequency = read <$> findField "freq"
                         }
    , Log.report     = Just Log.Report { Log.sent = findField "rst_sent"
                                       , Log.rcvd = findField "rst_rcvd"
                                       }
    }
   where
    findField :: String -> Maybe String
    findField key = do
      Adif.Field (_, val) <- find (\(Adif.Field (key', _)) -> key == key')
                                  fields
      return val
