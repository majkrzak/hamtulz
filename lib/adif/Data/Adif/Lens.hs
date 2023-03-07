module Data.Adif.Lens (module Data.Adif.Lens) where

import Control.Lens (DefName (TopName), lensField, lensRules, makeLensesWith, (&), (.~))
import Data.Adif.Model (Document, Header, Record)
import Language.Haskell.TH (mkName, nameBase)

makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Document
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Header
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Record
