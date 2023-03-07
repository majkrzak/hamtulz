module Data.Adi.Model (Field (Field), Record (Record), Header (Header), Document (Document)) where

import Data.Adi.Class (Adi')
import GHC.Generics (Generic)

newtype Field = Field (String, String) deriving (Eq, Generic, Adi')

newtype Record = Record [Field] deriving (Eq, Generic, Adi')

newtype Header = Header (String, [Field]) deriving (Eq, Generic, Adi')

newtype Document = Document (Maybe Header, [Record]) deriving (Eq, Generic, Adi')
