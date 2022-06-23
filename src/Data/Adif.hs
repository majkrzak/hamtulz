module Data.Adif where

newtype File = File (Maybe Header, [Record]) deriving Show
newtype Header = Header (HeaderText, [Field]) deriving Show
newtype Record = Record [Field] deriving Show
newtype Field = Field (FieldName, FieldData) deriving Show
type HeaderText = String
type FieldName = String
type FieldData = String
