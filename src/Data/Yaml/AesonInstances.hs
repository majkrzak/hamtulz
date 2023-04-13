module Data.Yaml.AesonInstances () where

import Data.Log (Band, Connection, Contacted, Location, Logging, Mode, Operator, Program, Record, Report, Station, Stations)
import Data.Text (unpack)
import Data.Yaml (FromJSON (parseJSON), withText)

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
