module Data.Adi.Lens (header, records, text, fields, name, payload) where

import Control.Lens (DefName (TopName), Lens', lens, lensField, lensRules, makeLensesWith, (&), (.~), _1)
import Data.Adi.Model (Document (..), Field (..), Header (..), Record (..))

header :: Lens' Document (Maybe Header)
header =
  lens
    (\(Document (x, _)) -> x)
    (\(Document (x, y)) x' -> Document (x', y))

records :: Lens' Document [Record]
records =
  lens
    (\(Document (_, y)) -> y)
    (\(Document (x, y)) y' -> Document (x, y'))

text :: Lens' Header String
text =
  lens
    (\(Header (x, _)) -> x)
    (\(Header (x, y)) x' -> Header (x', y))

class HasFields a where
  fields :: Lens' a [Field]

instance HasFields Header where
  fields =
    lens
      (\(Header (_, y)) -> y)
      (\(Header (x, y)) y' -> Header (x, y'))

instance HasFields Record where
  fields =
    lens
      (\(Record x) -> x)
      (\(Record x) x' -> Record x')

name :: Lens' Field String
name =
  lens
    (\(Field (x, _)) -> x)
    (\(Field (x, y)) x' -> Field (x', y))

payload :: Lens' Field String
payload =
  lens
    (\(Field (_, y)) -> y)
    (\(Field (x, y)) y' -> Field (x, y'))
