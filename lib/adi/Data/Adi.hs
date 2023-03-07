module Data.Adi (Field (Field), Record (Record), Header (Header), Document (Document), Adi, fromAdi, toAdi) where

import Data.Adi.Class (Adi, fromAdi, toAdi)
import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))
import Data.Adi.Read ()
import Data.Adi.Show ()
import Data.Adi.Valid ()
