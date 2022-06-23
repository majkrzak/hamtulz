module Data.Log where

import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Record = Record
  { datetime :: UTCTime
  , stations :: Maybe Stations
  , mode :: Maybe Mode
  , frequency :: Maybe Float
  , report :: Maybe Report
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Stations = Stations
  { logging :: Maybe Station
  , contacted :: Maybe Station
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Station = Station
  { callsign :: Maybe String
  , operator :: Maybe Operator
  , location :: Maybe Location
  , antenna :: Maybe Antenna
  , power :: Maybe Int
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Mode = CW | SSB | FT8 deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Report = Report
  { sent :: Maybe String
  , rcvd :: Maybe String
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Location = Location
  { gridsquare :: Maybe String
  , description :: Maybe String
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Antenna = Antenna
  { kind :: Maybe AntennaKind
  , orientation :: Maybe AntennaOrientation
  , length :: Maybe Int
  , azimuth :: Maybe Int
  , elevation :: Maybe Int
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data AntennaKind = Delta | Dipole | FoldedDipole | Wire deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
data AntennaOrientation = Horizontal | Vertical deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Operator = Operator
  { name :: Maybe String
  , age :: Maybe Int
  } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
