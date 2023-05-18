module Main where

import Control.Lens
import Control.Lens.Helper ((°), (·))
import Data.ByteString.Char8 qualified as BS8
import Data.Empty (Empty, empty)
import Data.Log
import Data.Maybe (isJust)
import Data.Text.Lens (packed)
import Data.Time (Day, UTCTime, getCurrentTime, utctDay, utctDayTime)
import Data.Time.LocalTime (timeOfDayToTime, timeToTimeOfDay)
import Data.Yaml.Builder (toByteString)
import Data.Yaml.Generic ()
import Data.Yaml.Instances ()
import GHC.Generics (Generic)
import Monomer
import System.Environment (getArgs)

data Template
  = EmptyRecord
  | ContestRecord
  deriving (Eq, Show)

data Remember = Remember
  { _rememberDate :: Bool,
    _rememberReport :: Bool,
    _rememberConnection :: Bool
  }
  deriving (Eq, Show, Generic, Empty)

makeLenses 'Remember

data AppModel = AppModel
  { _logFile :: String,
    _record :: Record,
    _remember :: Remember,
    _template :: Template
  }
  deriving (Eq, Show)

makeLenses 'AppModel

data AppEvent
  = AppInit
  | -- | AppToday
    -- | AppSetToday Day
    -- | AppNow
    -- | AppSetNow UTCTime
    AppSave
  | AppWriteRecord ()
  deriving (Eq, Show)

toRecord :: Template -> Record
toRecord EmptyRecord = empty
toRecord ContestRecord =
  [ stations ° contacted ° callsign ?~ "",
    connection ° frequency ?~ 0,
    report ° sent ?~ "59",
    report ° rcvd ?~ "59"
  ]
    · empty

enablable ::
  (WidgetEvent e, Empty s, CompositeModel s, CompParentModel s') =>
  ALens' s' (Maybe s) ->
  WidgetNode s e ->
  WidgetNode s' e
enablable lens' widget' =
  composite
    "enablable"
    lens'
    ( \wenv model ->
        hstack
          [ checkbox lens'',
            widgetIf
              (isJust model)
              ( composite
                  "enablable"
                  (non empty)
                  (\wenv model -> widget')
                  report_up
              )
          ]
    )
    report_up
  where
    lens'' =
      lens
        isJust
        ( curry $
            \case
              (_, False) -> Nothing
              (Nothing, True) -> Just empty
              (x, True) -> x
        )
    report_up _ _ _ evt = [Report evt]

buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    widgetTree =
      vstack
        [ hstack
            [ label "Day:",
              dateField $ record . datetime . lens utctDay (\x y -> x {utctDay = y}),
              -- button "Today" AppToday,
              label "Time:",
              timeField $ record . datetime . lens (timeToTimeOfDay . utctDayTime) (\x y -> x {utctDayTime = timeOfDayToTime y}) -- ,
              -- button "Now" AppNow
            ],
          hstack
            [ label "stations",
              enablable (record . stations) $
                vstack
                  [ hstack
                      [ label "contacted",
                        enablable contacted $
                          vstack
                            [ hstack
                                [ label "callsign",
                                  enablable (coerced . callsign) $ textField id
                                ],
                              hstack
                                [ label "operator",
                                  enablable (coerced . operator) $
                                    hstack
                                      [ label "name",
                                        enablable name $ textField id
                                      ]
                                ],
                              hstack
                                [ label "location",
                                  enablable (coerced . location) $
                                    vstack
                                      [ hstack
                                          [ label "gridsquare",
                                            enablable gridsquare $ textField id
                                          ],
                                        hstack
                                          [ label "description",
                                            enablable description $ textField id
                                          ],
                                        hstack
                                          [ label "program",
                                            enablable program $
                                              vstack
                                                [ hstack
                                                    [ label "sota",
                                                      enablable sota $ textField id
                                                    ],
                                                  hstack
                                                    [ label "wwff",
                                                      enablable wwff $ textField id
                                                    ]
                                                ]
                                          ]
                                      ]
                                ]
                            ]
                      ]
                  ]
            ],
          hstack
            [ label "connection",
              enablable (record . connection) $
                vstack
                  [ hstack
                      [ label "frequency",
                        enablable frequency $ numericField_ id [decimals 5]
                      ],
                    hstack
                      [ label "frequency_rx",
                        enablable frequency_rx $ numericField_ id [decimals 5]
                      ]
                  ]
            ],
          hstack
            [ label "report",
              enablable (record . report) $
                vstack
                  [ hstack
                      [ label "sent",
                        enablable sent $ textField id
                      ],
                    hstack
                      [ label "rcvd",
                        enablable rcvd $ textField id
                      ]
                  ]
            ],
          button "Save" AppSave,
          hstack
            [ label "Remember Date",
              checkbox $ remember . rememberDate,
              label "Remember Report",
              checkbox $ remember . rememberReport,
              label "Remember Connection",
              checkbox $ remember . rememberConnection
            ],
          hstack
            [ label "Template",
              dropdown
                template
                [EmptyRecord, ContestRecord]
                ( \case
                    EmptyRecord -> label "empty"
                    ContestRecord -> label "contest"
                )
                ( \case
                    EmptyRecord -> label "empty"
                    ContestRecord -> label "contest"
                )
            ],
          button "Clear" AppInit
        ]

handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit ->
    [ Model
        ( [ record .~ toRecord (model ^. template),
            if model ^. remember . rememberDate then record . datetime .~ (model ^. record . datetime) else id,
            if model ^. remember . rememberReport then record . report .~ (model ^. record . report) else id,
            if model ^. remember . rememberConnection then record . connection .~ (model ^. record . connection) else id
          ]
            · model
        )
    ]
  -- AppToday -> [Task $ AppSetToday . utctDay <$> getCurrentTime]
  -- AppSetToday x -> [Model (model {_record = (_record model) {Log.datetime = (Log.datetime $ _record model) {utctDay = x}}})]
  -- AppNow -> [Task $ AppSetNow <$> getCurrentTime]
  -- AppSetNow x -> [Model (model {_record = (_record model) {Log.datetime = x}})]
  AppSave -> [Task $ AppWriteRecord <$> BS8.appendFile (model ^. logFile) (toByteString [model ^. record]), Event AppInit]
  AppWriteRecord () -> []

main :: IO ()
main = do
  [logFile] <- getArgs
  startApp
    (AppModel logFile empty empty EmptyRecord)
    handleEvent
    buildUI
    [ appWindowTitle "Contest Log",
      appTheme darkTheme,
      appFontDef "Regular" "/usr/share/fonts/TTF/Hack-Regular.ttf",
      appInitEvent AppInit
    ]
