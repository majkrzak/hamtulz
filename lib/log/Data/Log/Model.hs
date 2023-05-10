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
    Document (..),
    Metadata (..),
  )
where

import Data.Empty (Empty)
import Data.Log.Enum (Band, Mode)
import {-# SOURCE #-} Data.Log.NewType (Contacted, Logging)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Document = Document
  { metadata :: Maybe Metadata,
    contacts :: Maybe [Record]
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Metadata = Metadata
  { callsigns :: Maybe [String],
    locations :: Maybe [Location],
    loggings :: Maybe [Logging]
  }
  deriving (Eq, Show, Read, Generic, Empty)

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
  { dxcc :: Maybe Int,
    gridsquare :: Maybe String,
    description :: Maybe String,
    program :: Maybe Program
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Program = Program
  { pga :: Maybe String,
    sota :: Maybe String,
    pota :: Maybe String,
    iota :: Maybe String,
    wwff :: Maybe String,
    wca :: Maybe String
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Operator = Operator
  { name :: Maybe String,
    age :: Maybe Int
  }
  deriving (Eq, Show, Read, Generic, Empty)
