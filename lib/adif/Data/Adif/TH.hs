module Data.Adif.TH (mkFieldName) where

import Language.Haskell.TH (mkName, Name)
import Data.Char (toLower)

mkFieldName :: String -> Name
mkFieldName field = mkName $ '_':(toLower <$> field)
