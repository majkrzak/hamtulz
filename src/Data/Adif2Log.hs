module Data.Adif2Log (adif2log) where

import Control.Lens (non, set, view, (^.))
import Data.Adif qualified as Adif
import Data.Empty (Empty, empty)
import Data.Log qualified as Log
import Data.Log.Lens qualified as Log'
import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)

adif2log :: [Adif.Record] -> [Log.Record]
adif2log =
  map
    ( \record' ->
        apply
          [ set Log'.datetime (parseTimeOrError True defaultTimeLocale "%Y%m%d%H%M%S" (fromJust (record' ^. Adif._qso_date) <> fromJust (record' ^. Adif._time_on)) :: UTCTime),
            set (Log'.stations . non empty . Log'.contacted . non empty . Log'.callsign) (record' ^. Adif._call),
            set (Log'.stations . non empty . Log'.contacted . non empty . Log'.location . non empty . Log'.gridsquare) (record' ^. Adif._gridsquare),
            set (Log'.connection . non empty . Log'.band) (read <$> record' ^. Adif._band),
            set (Log'.connection . non empty . Log'.mode) (read <$> record' ^. Adif._mode),
            set (Log'.connection . non empty . Log'.frequency) (read <$> record' ^. Adif._freq),
            set (Log'.report . non empty . Log'.sent) (record' ^. Adif._rst_sent),
            set (Log'.report . non empty . Log'.rcvd) (record' ^. Adif._rst_rcvd),
            set (Log'.stations . non empty . Log'.contacted . non empty . Log'.location . non empty . Log'.program . non empty . Log'.sota) (record' ^. Adif._sota_ref),
            id
          ]
          empty
    )

apply :: Empty a => [a -> a] -> a -> a
apply = foldl (.) id
