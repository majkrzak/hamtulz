module Data.Empty (Empty, empty) where

import Data.Time (UTCTime)
import GHC.Generics

class GEmpty f where
  gempty :: f a

instance GEmpty U1 where
  gempty = U1

instance (Empty a) => GEmpty (K1 i a) where
  gempty = K1 empty

instance (GEmpty a, GEmpty b) => GEmpty (a :*: b) where
  gempty = gempty :*: gempty

instance (GEmpty a, GEmpty b) => GEmpty (a :+: b) where
  gempty = L1 gempty

instance (GEmpty a) => GEmpty (M1 i c a) where
  gempty = M1 gempty

class Empty a where
  empty :: a
  default empty :: (Generic a, GEmpty (Rep a)) => a
  empty = to gempty

instance Empty (Maybe a) where
  empty = Nothing

instance Empty [a] where
  empty = []

instance (Empty a1, Empty a2) => Empty (a1,a2) where
  empty = (empty, empty)

instance {-# OVERLAPPABLE #-} Num a => Empty a where
  empty = 0

instance Empty UTCTime where
  empty = read "1970-01-01 00:00:00.000000 UTC"
