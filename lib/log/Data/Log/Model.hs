module Data.Log.Model
  ( Connection (..),
    Via (..),
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
import Data.Radio (Band, Locator, Mode, Satellite)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Document = Document
  { metadata :: Maybe Metadata,
    contacts :: Maybe [Record]
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Metadata = Metadata
  { callsigns :: Maybe [Text],
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

newtype Logging = Logging Station deriving (Eq, Show, Read, Generic, Empty)

newtype Contacted = Contacted Station deriving (Eq, Show, Read, Generic, Empty)

data Station = Station
  { callsign :: Maybe Text,
    operator :: Maybe Operator,
    location :: Maybe Location
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Connection = Connection
  { band :: Maybe Band,
    band_rx :: Maybe Band,
    mode :: Maybe Mode,
    frequency :: Maybe Double,
    frequency_rx :: Maybe Double,
    via :: Maybe Via
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Via = Via
  { satellite :: Maybe Satellite,
    repeater :: Maybe Text,
    talkgroup :: Maybe Text
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Report = Report
  { sent :: Maybe Text,
    rcvd :: Maybe Text
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Location = Location
  { dxcc :: Maybe Int,
    gridsquare :: Maybe Locator,
    description :: Maybe Text,
    program :: Maybe Program
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Program = Program
  { pga :: Maybe Text,
    sota :: Maybe Text,
    pota :: Maybe Text,
    iota :: Maybe Text,
    wwff :: Maybe Text,
    wca :: Maybe Text
  }
  deriving (Eq, Show, Read, Generic, Empty)

data Operator = Operator
  { name :: Maybe Text,
    age :: Maybe Int
  }
  deriving (Eq, Show, Read, Generic, Empty)
