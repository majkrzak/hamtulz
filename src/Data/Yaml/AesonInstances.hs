module Data.Yaml.AesonInstances () where

import Data.Log (Connection, Contacted, Document, Location, Logging, Metadata, Operator, Program, Record, Report, Station, Stations)
import Data.Radio (Band, Mode)
import Data.Text (unpack)
import Data.Yaml (FromJSON (parseJSON), withText)

deriving instance FromJSON Document

deriving instance FromJSON Metadata

deriving instance FromJSON Record

deriving instance FromJSON Stations

deriving instance FromJSON Logging

deriving instance FromJSON Contacted

deriving instance FromJSON Station

deriving instance FromJSON Operator

deriving instance FromJSON Location

deriving instance FromJSON Program

deriving instance FromJSON Connection

instance FromJSON Band where
  parseJSON = withText "Band" (pure . read . unpack)

deriving instance FromJSON Mode

deriving instance FromJSON Report
