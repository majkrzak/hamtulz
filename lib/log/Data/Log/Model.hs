module Data.Log.Model
  ( Connection (..),
    Location (..),
    Operator (..),
    Program (..),
    Record (..),
    Report (..),
    Station (..),
    Logging (..),
    Contacted (..),
    Stations (..),
  )
where

import Data.Empty (Empty)
import Data.Log.Enum (Band, Mode)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Record = Record
  { datetime :: UTCTime,
    stations :: Maybe Stations,
    connection :: Maybe Connection,
    report :: Maybe Report
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Stations = Stations
  { logging :: Maybe Logging,
    contacted :: Maybe Contacted
  }
  deriving (Eq, Show, Read, Generic, Empty)

newtype Logging = Logging Station deriving (Eq, Show, Read, Generic, Empty)

newtype Contacted = Contacted Station deriving (Eq, Show, Read, Generic, Empty)

data Station = Station
  { callsign :: Maybe String,
    operator :: Maybe Operator,
    location :: Maybe Location
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Connection = Connection
  { band :: Maybe Band,
    mode :: Maybe Mode,
    frequency :: Maybe Double
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Report = Report
  { sent :: Maybe String,
    rcvd :: Maybe String
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Location = Location
  { gridsquare :: Maybe String,
    description :: Maybe String,
    program :: Maybe Program
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Program = Program
  { sota :: Maybe String,
    pota :: Maybe String,
    wwff :: Maybe String,
    wca :: Maybe String
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Operator = Operator
  { name :: Maybe String,
    age :: Maybe Int
  }
  deriving (Eq, Show, Read, Generic, Empty)
