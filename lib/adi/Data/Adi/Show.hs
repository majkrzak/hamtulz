module Data.Adi.Show () where

import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))

instance Show Field where
  show (Field (name, payload)) = "<" <> name <> ":" <> show (length payload) <> ">" <> payload <> " "

instance {-# OVERLAPPING #-} Show [Field] where
  show (field : fields) = show field <> " " <> show fields
  show [] = mempty

instance Show Record where
  show (Record fields) = show fields <> "<eor>"

instance {-# OVERLAPPING #-} Show [Record] where
  show (record : records) = show record <> "\n" <> show records

instance Show Header where
  show (Header (free, fields)) = free <> "\n" <> show fields <> "<eoh>"

instance {-# OVERLAPPING #-} Show (Maybe Header) where
  show (Just header) = show header
  show Nothing = mempty

instance Show Document where
  show (Document (header, records)) = show header <> show records
