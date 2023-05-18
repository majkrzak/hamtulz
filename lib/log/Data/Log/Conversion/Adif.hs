module Data.Log.Conversion.Adif (toAdif, fromAdif) where

import Control.Lens (Lens', lens, set, view, (^.))
import Control.Lens.Helper (maybe', mrs, mpu, (°), (·))
import Data.Adif qualified as Adif
import Data.Empty (Empty, empty)
import Data.Log.Lens qualified as Log'
import Data.Log.Model qualified as Log
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
    (Log'.stations ° Log'.logging ° Log'.callsign . mpu, Adif._station_callsign),
    (Log'.stations ° Log'.contacted ° Log'.callsign . mpu, Adif._call),
    -- DXCC
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.dxcc . mrs, Adif._my_dxcc),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.dxcc . mrs, Adif._dxcc),
    -- Gridsquares
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.gridsquare . mrs, Adif._my_gridsquare),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.gridsquare . mrs, Adif._gridsquare),
    -- Report
    (Log'.report ° Log'.sent . mpu, Adif._rst_sent),
    (Log'.report ° Log'.rcvd . mpu, Adif._rst_rcvd),
    -- Connection
    (Log'.connection ° Log'.band . mrs, Adif._band),
    (Log'.connection ° Log'.band_rx . mrs, Adif._band_rx),
    (Log'.connection ° Log'.mode . mrs, Adif._mode),
    (Log'.connection ° Log'.frequency . mrs, Adif._freq),
    (Log'.connection ° Log'.frequency_rx . mrs, Adif._freq_rx),
    -- Programs
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.program ° Log'.sota . mpu, Adif._my_sota_ref),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.program ° Log'.sota . mpu, Adif._sota_ref),
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.program ° Log'.pota . mpu, Adif._my_pota_ref),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.program ° Log'.pota . mpu, Adif._pota_ref),
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.program ° Log'.iota . mpu, Adif._my_iota),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.program ° Log'.iota . mpu, Adif._iota),
    (Log'.stations ° Log'.logging ° Log'.location ° Log'.program ° Log'.wwff . mpu, Adif._my_wwff_ref),
    (Log'.stations ° Log'.contacted ° Log'.location ° Log'.program ° Log'.wwff . mpu, Adif._wwff_ref)
  ]
