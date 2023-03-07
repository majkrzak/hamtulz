module Data.Adi.Class (Adi', Adi, fromAdi, toAdi) where

class Adi' a'

class Adi' a' => Adi a' a where
  fromAdi :: a' -> a
  toAdi :: a -> a'
