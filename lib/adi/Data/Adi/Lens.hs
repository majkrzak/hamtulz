module Data.Adi.Lens (header, records, text, fields, name, payload) where

import Control.Lens (Lens', Wrapped, lens, _1, _2, _Wrapped')
import Data.Adi.Model (Document (..), Field (..), Header (..), Record (..))

instance Wrapped Document

instance Wrapped Field

instance Wrapped Header

instance Wrapped Record

header :: Lens' Document (Maybe Header)
header = _Wrapped' . _1

records :: Lens' Document [Record]
records = _Wrapped' . _2

text :: Lens' Header String
text = _Wrapped' . _1

class HasFields a where
  fields :: Lens' a [Field]

instance HasFields Header where
  fields = _Wrapped' . _2

instance HasFields Record where
  fields = _Wrapped'

name :: Lens' Field String
name = _Wrapped' . _1

payload :: Lens' Field String
payload = _Wrapped' . _2
