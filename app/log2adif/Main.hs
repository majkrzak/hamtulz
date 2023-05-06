module Main where

import Control.Lens (coerced, (.~), (?~), (^.), (^?), (^?!), _Just)
import Control.Lens.Helper ((·))
import Data.Adif qualified as Adif
import Data.Empty (empty)
import Data.List (groupBy)
import Data.Log as Log
import Data.Maybe (fromMaybe, isJust)
import Data.Time (utctDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml (decodeFileThrow)
import Data.Yaml.AesonInstances ()
import System.Environment (getArgs)

main :: IO ()
main = do
  [logFile, adifFile] <- getArgs
  document :: Log.Document <- decodeFileThrow logFile
  let records = document ^?! contacts . _Just

  mapM_ (\records' -> writeFile (adifFile ++ callsignFileName (head records')) (show $ Adif.toAdi $ makeAdif records')) (callsignRecords records)
  mapM_ (\records' -> writeFile (adifFile ++ sotaFileName (head records')) (show $ Adif.toAdi $ makeAdif records')) (sotaRecords records)
  mapM_ (\records' -> writeFile (adifFile ++ potaFileName (head records')) (show $ Adif.toAdi $ makeAdif records')) (potaRecords records)

makeAdif :: [Log.Record] -> Adif.Document
makeAdif records =
  [ Adif.header
      ?~ ( [ Adif.text .~ "Hamtulz generated log file",
             Adif._adif_ver ?~ "3.1.4"
           ]
             · empty
         ),
    Adif.records .~ toAdif records
  ]
    · empty

callsignFileName :: Log.Record -> String
callsignFileName record =
  safeStroke (record ^?! (Log.stations . _Just . Log.logging . _Just . coerced . Log.callsign . _Just)) ++ ".adif"

callsignRecords :: [Log.Record] -> [[Log.Record]]
callsignRecords = groupBy (\r1 r2 -> callsignFileName r1 == callsignFileName r2)

sotaFileName :: Log.Record -> String
sotaFileName record =
  concat
    [ "SOTA",
      "/",
      iso8601Show (utctDay (record ^. Log.datetime)),
      "_",
      safeStroke (record ^?! (Log.stations . _Just . Log.logging . _Just . coerced . Log.callsign . _Just)),
      "_",
      safeStroke (fromMaybe "" (record ^? (Log.stations . _Just . Log.logging . _Just . coerced . Log.location . _Just . Log.program . _Just . Log.sota . _Just))),
      ".adif"
    ]

sotaFilter :: Log.Record -> Bool
sotaFilter record =
  isJust (record ^? (Log.stations . _Just . Log.logging . _Just . coerced . Log.location . _Just . Log.program . _Just . Log.sota . _Just))
    || isJust (record ^? (Log.stations . _Just . Log.contacted . _Just . coerced . Log.location . _Just . Log.program . _Just . Log.sota . _Just))

sotaRecords :: [Log.Record] -> [[Log.Record]]
sotaRecords = groupBy (\r1 r2 -> sotaFileName r1 == sotaFileName r2) . filter sotaFilter

potaFileName :: Log.Record -> String
potaFileName record =
  concat
    [ "POTA",
      "/",
      iso8601Show (utctDay (record ^. Log.datetime)),
      "_",
      safeStroke (record ^?! (Log.stations . _Just . Log.logging . _Just . coerced . Log.callsign . _Just)),
      "_",
      safeStroke (fromMaybe "" (record ^? (Log.stations . _Just . Log.logging . _Just . coerced . Log.location . _Just . Log.program . _Just . Log.pota . _Just))),
      ".adif"
    ]

potaFilter :: Log.Record -> Bool
potaFilter record =
  isJust (record ^? (Log.stations . _Just . Log.logging . _Just . coerced . Log.location . _Just . Log.program . _Just . Log.pota . _Just))
    || isJust (record ^? (Log.stations . _Just . Log.contacted . _Just . coerced . Log.location . _Just . Log.program . _Just . Log.pota . _Just))

potaRecords :: [Log.Record] -> [[Log.Record]]
potaRecords = groupBy (\r1 r2 -> potaFileName r1 == potaFileName r2) . filter potaFilter

safeStroke :: String -> String
safeStroke =
  map $ \case
    '/' -> '-'
    c -> c
