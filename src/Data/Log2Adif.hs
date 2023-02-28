module Data.Log2Adif (log2adif) where

import qualified Data.Adif as Adif
import qualified Data.Log as Log
import Data.Time (formatTime, defaultTimeLocale)

-- | Converts internal log format to ADIF one.
-- With minimal set of required fields defined in:
-- https://lotw.arrl.org/lotw-help/submitting-qsos/
-- and frequency.
log2adif :: [Log.Record] -> [Adif.Record]
log2adif = map record2record
  where
    record2record :: Log.Record -> Adif.Record
    record2record r = Adif.emptyRecord
      { Adif._station_callsign = pure r >>= Log.stations >>= Log.logging >>= Log.callsign
      , Adif._call = pure r >>= Log.stations >>= Log.contacted >>= Log.callsign
      , Adif._qso_date = pure $ formatTime defaultTimeLocale "%Y%m%d" $ Log.datetime r
      , Adif._time_on = pure $ formatTime defaultTimeLocale "%H%M%S" $ Log.datetime r
      , Adif._band = show <$> (pure r >>= Log.connection >>= Log.band)
      , Adif._freq = show <$> (pure r >>= Log.connection >>= Log.frequency)
      , Adif._mode = show <$> (pure r >>= Log.connection >>= Log.mode)
      , Adif._my_sota_ref = pure r >>= Log.stations >>= Log.logging >>= Log.location >>= Log.program >>= Log.sota
      , Adif._sota_ref = pure r >>= Log.stations >>= Log.contacted >>= Log.location >>= Log.program >>= Log.sota
      , Adif._my_pota_ref = pure r >>= Log.stations >>= Log.logging >>= Log.location >>= Log.program >>= Log.pota
      , Adif._pota_ref = pure r >>= Log.stations >>= Log.contacted >>= Log.location >>= Log.program >>= Log.pota
      , Adif._my_wwff_ref = pure r >>= Log.stations >>= Log.logging >>= Log.location >>= Log.program >>= Log.wwff
      , Adif._wwff_ref = pure r >>= Log.stations >>= Log.contacted >>= Log.location >>= Log.program >>= Log.wwff
      }
