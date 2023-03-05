module Data.Adi.Model (Field (Field), Record (Record), Header (Header), Document (Document)) where

import GHC.Generics (Generic)

newtype Field = Field (String, String) deriving (Eq, Generic)

newtype Record = Record [Field] deriving (Eq, Generic)

newtype Header = Header (String, [Field]) deriving (Eq, Generic)

newtype Document = Document (Maybe Header, [Record]) deriving (Eq, Generic)
