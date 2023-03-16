module Data.Adi.Model
  ( Field (..),
    Record (..),
    Header (..),
    Document (..),
  )
where

import Data.Empty (Empty)
import GHC.Generics (Generic)

newtype Field = Field (String, String) deriving (Eq, Generic, Empty)

newtype Record = Record [Field] deriving (Eq, Generic, Empty)

newtype Header = Header (String, [Field]) deriving (Eq, Generic, Empty)

newtype Document = Document (Maybe Header, [Record]) deriving (Eq, Generic, Empty)
