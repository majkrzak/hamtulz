module Main where

import Control.Lens ((^.), (^?), (^?!), _Just)
import Data.Adif.Builder (toAdif)
import Data.List (groupBy)
import Data.Log as Log
import Data.Log.Lens as Log'
import Data.Log2Adif (log2adif)
import Data.Maybe (fromMaybe, isJust)
import Data.Time (utctDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml (decodeFileThrow)
import Data.Yaml.AesonInstances ()
import System.Environment (getArgs)

main :: IO ()
main = do
  [logFile, adifFile] <- getArgs
  records :: [Log.Record] <- decodeFileThrow logFile

  mapM_ (\records' -> writeFile (adifFile ++ callsignFileName (head records')) (toAdif $ log2adif records')) (callsignRecords records)
  mapM_ (\records' -> writeFile (adifFile ++ sotaFileName (head records')) (toAdif $ log2adif records')) (sotaRecords records)
  mapM_ (\records' -> writeFile (adifFile ++ potaFileName (head records')) (toAdif $ log2adif records')) (potaRecords records)

callsignFileName :: Log.Record -> String
callsignFileName record =
  safeStroke (record ^?! (Log'.stations . _Just . Log'.logging . _Just . Log'.callsign . _Just)) ++ ".adif"

callsignRecords :: [Log.Record] -> [[Log.Record]]
callsignRecords = groupBy (\r1 r2 -> callsignFileName r1 == callsignFileName r2)

sotaFileName :: Log.Record -> String
sotaFileName record =
  concat
    [ "SOTA",
      "/",
      iso8601Show (utctDay (record ^. Log'.datetime)),
      "_",
      safeStroke (record ^?! (Log'.stations . _Just . Log'.logging . _Just . Log'.callsign . _Just)),
      "_",
      safeStroke (fromMaybe "" (record ^? (Log'.stations . _Just . Log'.logging . _Just . Log'.location . _Just . Log'.program . _Just . Log'.sota . _Just))),
      ".adif"
    ]

sotaFilter :: Log.Record -> Bool
sotaFilter record =
  isJust (record ^? (Log'.stations . _Just . Log'.logging . _Just . Log'.location . _Just . Log'.program . _Just . Log'.sota . _Just))
    || isJust (record ^? (Log'.stations . _Just . Log'.contacted . _Just . Log'.location . _Just . Log'.program . _Just . Log'.sota . _Just))

sotaRecords :: [Log.Record] -> [[Log.Record]]
sotaRecords = groupBy (\r1 r2 -> sotaFileName r1 == sotaFileName r2) . filter sotaFilter

potaFileName :: Log.Record -> String
potaFileName record =
  concat
    [ "POTA",
      "/",
      iso8601Show (utctDay (record ^. Log'.datetime)),
      "_",
      safeStroke (record ^?! (Log'.stations . _Just . Log'.logging . _Just . Log'.callsign . _Just)),
      "_",
      safeStroke (fromMaybe "" (record ^? (Log'.stations . _Just . Log'.logging . _Just . Log'.location . _Just . Log'.program . _Just . Log'.pota . _Just))),
      ".adif"
    ]

potaFilter :: Log.Record -> Bool
potaFilter record =
  isJust (record ^? (Log'.stations . _Just . Log'.logging . _Just . Log'.location . _Just . Log'.program . _Just . Log'.pota . _Just))
    || isJust (record ^? (Log'.stations . _Just . Log'.contacted . _Just . Log'.location . _Just . Log'.program . _Just . Log'.pota . _Just))

potaRecords :: [Log.Record] -> [[Log.Record]]
potaRecords = groupBy (\r1 r2 -> potaFileName r1 == potaFileName r2) . filter potaFilter

safeStroke :: String -> String
safeStroke =
  map $ \case
    '/' -> '-'
    c -> c
