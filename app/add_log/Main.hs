{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Lens
import Monomer

import qualified Data.Log as Log
import Data.Log.Lens as Log'

import Data.Time (getCurrentTime, utctDay, Day, UTCTime, utctDayTime)
import Data.Time.LocalTime (timeToTimeOfDay, timeOfDayToTime)
import Data.Text.Lens (packed)
import Data.Empty (empty, Empty)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml.Builder (toByteString)
import Data.Yaml.Generic ()
import Data.Yaml.Instances ()
import Data.Maybe (isJust)


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



enablable ::
 (WidgetEvent e, Empty s, CompositeModel s, CompParentModel s')
 => ALens' s' (Maybe s)
 -> WidgetNode s e
 -> WidgetNode s' e
enablable lens' widget' =
  composite
  "enablable"
  lens'
  (\wenv model ->
    hstack
     [ checkbox lens''
     , widgetIf (isJust model) (
       composite
       "enablable"
       (non empty)
       (\wenv model -> widget')
       report_up
     )
     ]
  )
  report_up
  where
    lens'' = lens
                 isJust
                 (
                   curry $
                       \case
                         (_, False) -> Nothing
                         (Nothing, True) -> Just empty
                         (x, True) -> x
                 )
    report_up _ _ _ evt = [Report evt]

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
        [ label "stations"
        , enablable (record . stations) $
          vstack
          [
            hstack
            [ label station_label
            , enablable station_lens $ vstack
              [ hstack
                [ label "callsign"
                , enablable callsign $ textField packed
                ]
              , hstack
                [ label "operator"
                , enablable operator $ hstack
                  [ label "name"
                  , enablable name $ textField packed
                  ]
                ]
              , hstack
                [ label "location"
                , enablable location $ vstack
                  [ hstack
                    [ label "gridsquare"
                    , enablable gridsquare $ textField packed
                    ]
                  , hstack
                    [ label "description"
                    , enablable description $ textField packed
                    ]
                  , hstack
                    [ label "program"
                    , enablable program $ vstack
                      [ hstack
                        [ label "sota"
                        , enablable sota $ textField packed
                        ]
                      , hstack
                        [ label "wwff"
                        , enablable wwff $ textField packed
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          | (station_label, station_lens) <- [("logging", logging),("contacted", contacted)] ]

        ]
      , hstack
        [ label "connection"
        , enablable (record . connection) $ vstack
          [ hstack
            [ label "frequency"
            , enablable frequency $ numericField_ id [decimals 5]
            ]
          ]
        ]
      , hstack
        [ label "report"
        , enablable (record . report) $ vstack
          [ hstack
            [ label "sent"
            , enablable sent $ textField packed
            ]
          , hstack
            [ label "rcvd"
            , enablable rcvd $ textField packed
            ]
          ]
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
    [ appWindowTitle "Contest Log"
    , appTheme darkTheme
    , appFontDef "Regular" "/usr/share/fonts/TTF/Hack-Regular.ttf"
    , appInitEvent AppInit
    ]


data Nut
