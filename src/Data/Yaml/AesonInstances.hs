module Data.Yaml.AesonInstances () where

import Data.Yaml (FromJSON(parseJSON), withText)
import Data.Log (Record, Stations, Station, Operator, Location, Program, Antenna, AntennaKind, AntennaOrientation, Connection, Band, Mode, Report)
import Data.Text (unpack)

deriving instance FromJSON Record
deriving instance FromJSON Stations
deriving instance FromJSON Station
deriving instance FromJSON Operator
deriving instance FromJSON Location
deriving instance FromJSON Program
deriving instance FromJSON Antenna 
deriving instance FromJSON AntennaKind
deriving instance FromJSON AntennaOrientation
deriving instance FromJSON Connection
instance FromJSON Band where
  parseJSON = withText "Band" (pure . read . unpack) 
deriving instance FromJSON Mode
deriving instance FromJSON Report
