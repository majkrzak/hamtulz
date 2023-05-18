module Data.Log.Valid () where

import Data.Coerce (coerce)
import Data.Empty (empty)
import Data.Function (fix)
import Data.Log.Model
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Valid (Valid, mkLabel, mkListRecursiveValidator, mkMaybeRecursiveValidator, mkMaybeValidator, mkNested, mkRecursiveValidator, mkValidator, mkValidatorComment, validator)

instance Valid Document where
  validator =
    mconcat
      [ mkNested contacts $
          mconcat
            [ mkMaybeRecursiveValidator "?" id
            ]
      ]

instance Valid [Record] where
  validator =
    mconcat
      [ mkValidatorComment
          ( fix
              ( \f ->
                  \case
                    r1 : r2 : rs -> if datetime r1 >= datetime r2 then Just ("timestamps not in increesing order @" <> iso8601Show (datetime r2)) else f (r2 : rs)
                    [r] -> Nothing
                    [] -> Nothing
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
                ],
          mkLabel (const "connection") $
            mkNested connection $
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
          mkNested contacted $
            mconcat
              [ mkValidator "is empty" null,
                mkMaybeRecursiveValidator "?" id
              ]
      ]

instance Valid Logging where
  validator =
    mkRecursiveValidator "" (coerce :: (Logging -> Station))

instance Valid Contacted where
  validator =
    mkRecursiveValidator "" (coerce :: (Contacted -> Station))

instance Valid Station where
  validator =
    mconcat
      [ mkLabel (const "callsign") $
          mkNested callsign $
            mconcat
              [ mkValidator "is empty" null,
                mkMaybeValidator "is empty" (== empty)
              ]
      ]

instance Valid Connection where
  validator =
    mconcat
      [ mkLabel (const "frequency") $
          mkNested frequency $
            mconcat
              [ mkValidator "is empty" null
              ],
        mkLabel (const "mode") $
          mkNested mode $
            mconcat
              [ mkValidator "is empty" null
              ],
        mkLabel (const "band") $
          mkNested band $
            mconcat
              [ mkValidator "is empty" null
              ]
      ]
