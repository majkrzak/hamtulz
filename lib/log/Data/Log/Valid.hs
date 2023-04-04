module Data.Log.Valid () where

import Data.Function (fix)
import Data.Log.Model
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Valid (Valid, mkLabel, mkListRecursiveValidator, mkMaybeRecursiveValidator, mkMaybeValidator, mkNested, mkValidator, validator)

instance Valid [Record] where
  validator =
    mconcat
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
    mkLabel (\a -> "@" <> iso8601Show (datetime a)) $
      mconcat
        [ mkLabel (const "stations") $
            mkNested stations $
              mconcat
                [ mkValidator "is empty" null,
                  mkMaybeRecursiveValidator "?" id
                ]
        ]

instance Valid Stations where
  validator =
    mconcat
      [ mkLabel (const "logging") $
          mkNested logging $
            mconcat
              [ mkValidator "is empty" null,
                mkMaybeRecursiveValidator "?" id
              ],
        mkLabel (const "contacted") $
          mkNested logging $
            mconcat
              [ mkValidator "is empty" null,
                mkMaybeRecursiveValidator "?" id
              ]
      ]

instance Valid Station where
  validator =
    mconcat
      [ mkLabel (const "callsign") $
          mkNested callsign $
            mconcat
              [ mkValidator "is empty" null,
                mkMaybeValidator "is empty" null
              ]
      ]
