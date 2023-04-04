module Data.Log.Valid () where

import Data.Function (fix)
import Data.Log.Model
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Valid (Valid, mkGroup, mkListRecursiveValidator, mkMaybeRecursiveValidator, mkMaybeValidator, mkValidator, validator)

instance Valid [Record] where
  validator =
    mkGroup
      (const "")
      id
      [ mkValidator
          "timestamps not in increesing order"
          ( fix
              ( \f ->
                  \case
                    r1 : r2 : rs -> datetime r1 >= datetime r2 || f rs
                    [r] -> False
                    [] -> False
              )
          ),
        mkListRecursiveValidator "[]" id
      ]

instance Valid Record where
  validator =
    mkGroup
      (\a -> "@" <> iso8601Show (datetime a))
      id
      [ mkGroup
          (const "stations")
          stations
          [ mkValidator "is empty" null,
            mkMaybeRecursiveValidator "?" id
          ]
      ]

instance Valid Stations where
  validator =
    mkGroup
      (const "")
      id
      [ mkGroup
          (const "logging")
          logging
          [ mkValidator "is empty" null,
            mkMaybeRecursiveValidator "?" id
          ],
        mkGroup
          (const "contacted")
          logging
          [ mkValidator "is empty" null,
            mkMaybeRecursiveValidator "?" id
          ]
      ]

instance Valid Station where
  validator =
    mkGroup
      (const "")
      id
      [ mkGroup
          (const "callsign")
          callsign
          [ mkValidator "is empty" null,
            mkMaybeValidator "is empty" null
          ]
      ]
