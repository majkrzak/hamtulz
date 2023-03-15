module Data.Log.Adif (toAdif, fromAdif) where

import Control.Lens (Lens', lens, set, view, (^.))
import Control.Lens.Helper (maybe', mrs, (°), (·))
import Data.Adif qualified as Adif
import Data.Empty (Empty, empty)
import Data.Log qualified as Log
import Data.Log.Lens qualified as Log'
import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeOrError, utctDay, utctDayTime)

fromAdif :: [Adif.Record] -> [Log.Record]
fromAdif = map (\record' -> map (\(to, from) -> set to (record' ^. from)) converters · empty)

toAdif :: [Log.Record] -> [Adif.Record]
toAdif = map (\record' -> map (\(from, to) -> set to (record' ^. from)) converters · empty)

days :: Lens' UTCTime String
days =
  lens
    (formatTime defaultTimeLocale "%Y%m%d")
    (\time time' -> time {utctDay = utctDay (parseTimeOrError True defaultTimeLocale "%Y%m%d" time')})

hours :: Lens' UTCTime String
hours =
  lens
    (formatTime defaultTimeLocale "%H%M%S")
    (\time time' -> time {utctDayTime = utctDayTime (parseTimeOrError True defaultTimeLocale "%H%M%S" time')})

converters :: [(Lens' Log.Record (Maybe String), Lens' Adif.Record (Maybe String))]
converters =
  [ -- Date Time
    (Log'.datetime . days . maybe', Adif._qso_date),
    (Log'.datetime . hours . maybe', Adif._time_on),
    -- Callsigns
    (Log'.stations ° Log'.logging ° Log'.callsign, Adif._station_callsign),
    (Log'.stations ° Log'.contacted ° Log'.callsign, Adif._call),
    -- Gridsquares
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.gridsquare, Adif._my_gridsquare),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.gridsquare, Adif._gridsquare),
    -- Report
    (Log'.report ° Log'.sent, Adif._rst_sent),
    (Log'.report ° Log'.rcvd, Adif._rst_rcvd),
    -- Connection
    (Log'.connection ° Log'.band . mrs, Adif._band),
    (Log'.connection ° Log'.mode . mrs, Adif._band),
    (Log'.connection ° Log'.frequency . mrs, Adif._freq),
    -- Programs
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.program ° Log'.sota, Adif._my_sota_ref),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.program ° Log'.sota, Adif._sota_ref),
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.program ° Log'.pota, Adif._my_pota_ref),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.program ° Log'.pota, Adif._pota_ref),
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.program ° Log'.wwff, Adif._my_wwff_ref),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.program ° Log'.wwff, Adif._wwff_ref)
  ]
