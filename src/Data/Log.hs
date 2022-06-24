module Data.Log where

import Data.Time (UTCTime)
import GHC.Generics (Generic)


data Record = Record
  { datetime :: UTCTime
  , stations :: Maybe Stations
  , mode :: Maybe Mode
  , frequency :: Maybe Double
  , report :: Maybe Report
  } deriving (Eq, Show, Read, Generic)

data Stations = Stations
  { logging :: Maybe Station
  , contacted :: Maybe Station
  } deriving (Eq, Show, Read, Generic)

data Station = Station
  { callsign :: Maybe String
  , operator :: Maybe Operator
  , location :: Maybe Location
  , antenna :: Maybe Antenna
  , power :: Maybe Int
  } deriving (Eq, Show, Read, Generic)

data Mode = CW | SSB | FT8 deriving (Eq, Show, Read, Generic)

data Report = Report
  { sent :: Maybe String
  , rcvd :: Maybe String
  } deriving (Eq, Show, Read, Generic)

data Location = Location
  { gridsquare :: Maybe String
  , description :: Maybe String
  } deriving (Eq, Show, Read, Generic)

data Antenna = Antenna
  { kind :: Maybe AntennaKind
  , orientation :: Maybe AntennaOrientation
  , length :: Maybe Int
  , azimuth :: Maybe Int
  , elevation :: Maybe Int
  } deriving (Eq, Show, Read, Generic)

data AntennaKind = Delta | Dipole | FoldedDipole | Wire deriving (Eq, Show, Read, Generic)
data AntennaOrientation = Horizontal | Vertical deriving (Eq, Show, Read, Generic)

data Operator = Operator
  { name :: Maybe String
  , age :: Maybe Int
  } deriving (Eq, Show, Read, Generic)

