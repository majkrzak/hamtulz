module Data.Adif.TH (mkFieldName) where

import Data.Char (toLower)
import Language.Haskell.TH (Name, mkName)

mkFieldName :: String -> Name
mkFieldName field = mkName $ '_' : (toLower <$> field)
