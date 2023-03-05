module Data.Adi.Valid () where

import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))
import Data.Char (isAscii, isLetter, isSpace, isUpper, ord, toUpper)
import Data.List (isInfixOf)
import Data.Valid (Valid, valid)

instance Valid Field where
  valid (Field (name, payload)) = all (\c -> (isAscii c && isUpper c && isLetter c) || c == '_') name && not (null name)

instance Valid Record where
  valid (Record fields) = all valid fields && not (null fields)

instance Valid Header where
  valid (Header (text, fields)) =
    and
      [ not ("<EOH>" `isInfixOf` map toUpper text),
        not (null text),
        head text /= '<',
        all isAdiCharacter text,
        all valid fields,
        not (isSpace (head text)),
        not (isSpace (last text))
      ]

instance Valid Document where
  valid (Document (maybeHeader, records)) =
    all valid records && maybe True valid maybeHeader

isAdiCharacter :: Char -> Bool
isAdiCharacter c = 32 <= ord c && ord c <= 126

isAdiString :: String -> Bool
isAdiString = all isAdiCharacter
