module Main where

import Control.Lens (coerced, (.~), (?~), (^.), (^?), (^?!), _Just)
import Control.Lens.Helper (mpu, (°), (·))
import Data.Adif qualified as Adif
import Data.Empty (empty)
import Data.List (group, groupBy, sort)
import Data.Log as Log
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (splitOn, unpack)
import Data.Time (utctDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Yaml.Parser (readYamlFile)
import System.Environment (getArgs)

labels :: Log.Record -> [String]
labels record =
  mconcat
    [ ["all", callsign],
      ["SOTA" <> "/" <> date <> "_" <> callsign | isJust (record ^. Log.stations ° Log.contacted ° Log.location ° Log.program ° Log.sota) && isNothing my_summit],
      ["SOTA" <> "/" <> date <> "_" <> callsign <> "_" <> safeStroke summit | Just summit <- [my_summit]],
      ["POTA" <> "/" <> date <> "_" <> callsign <> "_" <> safeStroke park | park <- my_parks]
    ]
  where
    date = iso8601Show (utctDay (record ^. Log.datetime))
    callsign = safeStroke $ record ^?! Log.stations . _Just . Log.logging . _Just . coerced . Log.callsign . mpu . _Just
    my_summit = record ^. Log.stations ° Log.logging ° Log.location ° Log.program ° Log.sota . mpu
    my_parks = map unpack (maybe [] (splitOn ",") (record ^. Log.stations ° Log.logging ° Log.location ° Log.program ° Log.pota))

main :: IO ()
main = do
  [logFile, adifFile] <- getArgs
  document :: Log.Document <- readYamlFile logFile
  let records = document ^?! contacts . _Just
  let all_labels = (map head . group . sort) (mconcat (map labels records))

  mconcat [writeFile (adifFile <> label <> ".adi") (show $ Adif.toAdi $ makeAdif records label) | label <- all_labels]

makeAdif :: [Log.Record] -> String -> Adif.Document
makeAdif records label =
  [ Adif.header
      ?~ ( [ Adif.text .~ label,
             Adif._adif_ver ?~ "3.1.4",
             Adif._programid ?~ "Hamtulz"
           ]
             · empty
         ),
    Adif.records .~ toAdif (filter (elem label . labels) records)
  ]
    · empty

safeStroke :: String -> String
safeStroke =
  map $ \case
    '/' -> '-'
    c -> c
