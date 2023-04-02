module Data.Adi.Valid () where

import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))
import Data.Char (isAscii, isLetter, isSpace, isUpper, ord, toUpper)
import Data.List (isInfixOf)
import Data.Valid (Valid, mkGroup, mkListRecursiveValidator, mkMaybeRecursiveValidator, mkRecursiveValidator, mkValidator, valid, validators)

instance Valid Field where
  validators =
    [ mkGroup
        "name"
        (\(Field (name, _)) -> name)
        [ mkValidator "contains illegal characters" $ not . all (\c -> (isAscii c && isUpper c && isLetter c) || c == '_'),
          mkValidator "is empty" null
        ]
    ]

instance Valid Record where
  validators =
    [ mkGroup
        "fields"
        (\(Record fields) -> fields)
        [ mkValidator "is empty" null,
          mkListRecursiveValidator "[]" id
        ]
    ]

instance Valid Header where
  validators =
    [ mkGroup
        "text"
        (\(Header (text, _)) -> text)
        [ mkValidator "contains `<EOH>`" $ isInfixOf "<EOH>" . map toUpper,
          mkValidator "is empty" null,
          mkValidator "starts with `<`" $ ('<' ==) . head,
          mkValidator "starts with space" $ isSpace . head,
          mkValidator "ends with space" $ isSpace . last,
          mkValidator "not Adif String" $ not . isAdiString
        ],
      mkGroup
        "fields"
        (\(Header (_, fields)) -> fields)
        [ mkListRecursiveValidator "[]" id
        ]
    ]

instance Valid Document where
  validators =
    [ mkMaybeRecursiveValidator "header" (\(Document (maybeHeader, _)) -> maybeHeader),
      mkListRecursiveValidator "records" (\(Document (_, records)) -> records)
    ]

isAdiCharacter :: Char -> Bool
isAdiCharacter c = 32 <= ord c && ord c <= 126

isAdiString :: String -> Bool
isAdiString = all isAdiCharacter
