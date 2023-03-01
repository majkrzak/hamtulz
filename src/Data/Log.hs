module Data.Log where

import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Empty (Empty)


data Record = Record
  { datetime :: UTCTime
  , stations :: Maybe Stations
  , connection :: Maybe Connection
  , report :: Maybe Report
  } deriving (Eq, Show, Read, Generic, Empty)

data Stations = Stations
  { logging :: Maybe Station
  , contacted :: Maybe Station
  } deriving (Eq, Show, Read, Generic, Empty)

data Station = Station
  { callsign :: Maybe String
  , operator :: Maybe Operator
  , location :: Maybe Location
  , antenna :: Maybe Antenna
  , power :: Maybe Int
  } deriving (Eq, Show, Read, Generic, Empty)

data Connection = Connection
  { band :: Maybe Band
  , mode :: Maybe Mode
  , frequency :: Maybe Double
  } deriving (Eq, Show, Read, Generic, Empty)

data Band = Two | Six | Ten | Twelve | Fifteen | Seventeen | Twenty | Thirty | Forty | Eighty deriving (Eq, Generic, Empty)

instance Show Band where
  show Two = "2m"
  show Six = "6m"
  show Ten = "10m"
  show Twelve = "12m"
  show Fifteen = "15m"
  show Seventeen = "17m"
  show Twenty = "20m"
  show Thirty = "30m"
  show Forty = "40m"
  show Eighty = "80m"

instance Read Band where
  readsPrec _ "2m" = [(Two, "")]
  readsPrec _ "6m" = [(Six, "")]
  readsPrec _ "10m" = [(Ten, "")]
  readsPrec _ "12m" = [(Twelve, "")]
  readsPrec _ "15m" = [(Fifteen, "")]
  readsPrec _ "17m" = [(Seventeen, "")]
  readsPrec _ "20m" = [(Twenty, "")]
  readsPrec _ "30m" = [(Thirty, "")]
  readsPrec _ "40m" = [(Forty, "")]
  readsPrec _ "80m" = [(Eighty, "")]
  readsPrec _ _ = []

data Mode = CW | SSB | FM | FT8 deriving (Eq, Show, Read, Generic, Empty)

data Report = Report
  { sent :: Maybe String
  , rcvd :: Maybe String
  } deriving (Eq, Show, Read, Generic, Empty)

data Location = Location
  { gridsquare :: Maybe String
  , description :: Maybe String
  , program :: Maybe Program
  } deriving (Eq, Show, Read, Generic, Empty)

data Program = Program
  { sota :: Maybe String
  , pota :: Maybe String
  , wwff :: Maybe String
  , wca :: Maybe String
  } deriving (Eq, Show, Read, Generic, Empty)

data Antenna = Antenna
  { kind :: Maybe AntennaKind
  , orientation :: Maybe AntennaOrientation
  , length :: Maybe Int
  , azimuth :: Maybe Int
  , elevation :: Maybe Int
  } deriving (Eq, Show, Read, Generic, Empty)

data AntennaKind = Delta | Dipole | FoldedDipole | Wire deriving (Eq, Show, Read, Generic)
data AntennaOrientation = Horizontal | Vertical deriving (Eq, Show, Read, Generic)

data Operator = Operator
  { name :: Maybe String
  , age :: Maybe Int
  } deriving (Eq, Show, Read, Generic, Empty)
