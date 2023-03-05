module Data.Valid (Valid, valid) where

class Valid a where
  valid :: a -> Bool
