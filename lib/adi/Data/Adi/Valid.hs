module Data.Adi.Valid () where

import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))
import Data.Char (isAscii, isLetter, isSpace, isUpper, ord, toUpper)
import Data.List (isInfixOf)
import Data.Valid (Valid, mkLabel, mkListRecursiveValidator, mkMaybeRecursiveValidator, mkNested, mkRecursiveValidator, mkValidator, valid, validator)

instance Valid Field where
  validator =
    mconcat
      [ mkLabel (const "name") $
          mkNested (\(Field (name, _)) -> name) $
            mconcat
              [ mkValidator "contains illegal characters" $ not . all (\c -> (isAscii c && isUpper c && isLetter c) || c == '_'),
                mkValidator "is empty" null
              ]
      ]

instance Valid Record where
  validator =
    mconcat
      [ mkLabel (const "fields") $
          mkNested (\(Record fields) -> fields) $
            mconcat
              [ mkValidator "is empty" null,
                mkListRecursiveValidator "[]" id
              ]
      ]

instance Valid Header where
  validator =
    mconcat
      [ mkLabel (const "text") $
          mkNested (\(Header (text, _)) -> text) $
            mconcat
              [ mkValidator "contains `<EOH>`" $ isInfixOf "<EOH>" . map toUpper,
                mkValidator "is empty" null,
                mkValidator "starts with `<`" $ ('<' ==) . head,
                mkValidator "starts with space" $ isSpace . head,
                mkValidator "ends with space" $ isSpace . last,
                mkValidator "not Adif String" $ not . isAdiString
              ],
        mkLabel (const "fields") $
          mkNested (\(Header (_, fields)) -> fields) $
            mconcat
              [ mkListRecursiveValidator "[]" id
              ]
      ]

instance Valid Document where
  validator =
    mconcat
      [ mkMaybeRecursiveValidator "header" (\(Document (maybeHeader, _)) -> maybeHeader),
        mkListRecursiveValidator "records" (\(Document (_, records)) -> records)
      ]

isAdiCharacter :: Char -> Bool
isAdiCharacter c = 32 <= ord c && ord c <= 126

isAdiString :: String -> Bool
isAdiString = all isAdiCharacter
