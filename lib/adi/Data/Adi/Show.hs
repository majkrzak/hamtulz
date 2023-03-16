module Data.Adi.Show () where

import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))
import Data.List (intercalate, unwords)

instance Show Field where
  show (Field (name, payload)) = "<" <> name <> ":" <> show (length payload) <> ">" <> payload

instance {-# OVERLAPPING #-} Show [Field] where
  show fields = unwords $ map show fields

instance Show Record where
  show (Record fields) = show fields <> " " <> "<EOR>"

instance {-# OVERLAPPING #-} Show [Record] where
  show records = intercalate "\n" $ map show records

instance Show Header where
  show (Header (free, fields)) = free <> "\n" <> show fields <> "\n" <> "<EOH>"

instance {-# OVERLAPPING #-} Show (Maybe Header) where
  show (Just header) = show header
  show Nothing = mempty

instance Show Document where
  show (Document (header, records)) = show header <> "\n" <> show records <> "\n"
