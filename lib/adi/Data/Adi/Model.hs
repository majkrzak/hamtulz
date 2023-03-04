module Data.Adi.Model (Field (Field), Record (Record), Header (Header), Document (Document)) where

newtype Field = Field (String, String) deriving (Eq)

newtype Record = Record [Field] deriving (Eq)

newtype Header = Header (String, [Field]) deriving (Eq)

newtype Document = Document (Maybe Header, [Record]) deriving (Eq)
