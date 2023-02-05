{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Lens
import Monomer

import qualified Data.Log as Log
import Data.Log.Lens as Log'

import Data.Time (getCurrentTime, utctDay, Day, UTCTime, utctDayTime)
import Data.Time.LocalTime (timeToTimeOfDay, timeOfDayToTime)
import Data.Text.Lens (packed)
import Data.Empty (empty)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml.Builder (toByteString)
import Data.Yaml.Generic ()
import Data.Yaml.Instances ()


data AppModel = AppModel
 { _logFile :: String
 , _record :: Log.Record
 , _rememberDate :: Bool
 , _rememberReport :: Bool
 } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppToday
  | AppSetToday Day
  | AppNow
  | AppSetNow UTCTime
  | AppSave
  | AppWriteRecord ()
  deriving (Eq, Show)

makeLenses 'AppModel



buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree =
    vstack
      [ hstack
        [ label "Day:"
        , dateField $ record . datetime . lens utctDay (\x y -> x{utctDay=y})
        , button "Today" AppToday
        , label "Time:"
        , timeField  $ record . datetime . lens (timeToTimeOfDay . utctDayTime) (\x y -> x{utctDayTime= timeOfDayToTime y})
        , button "Now" AppNow
        ]
      , hstack
        [ label "Callsign"
        , textField $ record . stations . non empty . contacted . non empty . callsign . non "" . packed
        ]
      , hstack
        [ label "Report sent"
        , textField $ record . report . non empty . sent . non "" . packed
        ,  label "rcvd"
        , textField $ record . report . non empty . rcvd . non "" . packed
        ]
      , hstack
        [ label "Frequency"
        , numericField_ (record . connection . non empty . frequency . non 0) [decimals 5]
        ]
      , button "Save" AppSave
      , hstack
        [ label "Remember Date"
        , checkbox rememberDate
        ,  label "Remember Report"
        , checkbox rememberReport
        ]
      ]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit ->
    [ Model (model
        & (if model ^. rememberDate then record . datetime .~ model ^. record . datetime else id)
        . (if model ^. rememberReport then record . report . non empty . sent .~ model ^. record . report . non empty . sent else id)
        . (record .~ empty)
      ) ]
  AppToday -> [Task $ AppSetToday . utctDay <$> getCurrentTime]
  AppSetToday x -> [Model (model {_record = (_record model) { Log.datetime = (Log.datetime $ _record model){utctDay = x} } })]
  AppNow -> [Task $ AppSetNow <$> getCurrentTime]
  AppSetNow x ->  [Model (model {_record = (_record model) { Log.datetime = x } })]
  AppSave -> [ Task $ AppWriteRecord <$> BS8.appendFile (model ^. logFile) (toByteString [model ^. record]), Event AppInit]
  AppWriteRecord () -> []

main :: IO ()
main = do
  [logFile] <- getArgs
  startApp (AppModel logFile empty False False) handleEvent buildUI
    [ appRenderOnMainThread
    , appWindowTitle "Contest Log"
    , appTheme darkTheme
    , appFontDef "Regular" "/usr/share/fonts/TTF/Hack-Regular.ttf"
    , appInitEvent AppInit
    ]