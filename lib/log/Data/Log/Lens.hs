module Data.Log.Lens where

import Control.Lens (DefName (TopName), lensField, lensRules, makeLensesWith, (&), (.~))
import Data.Log.Model (Connection, Document, Location, Metadata, Operator, Program, Record, Report, Station, Stations)
import Language.Haskell.TH (mkName, nameBase)

makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Document

makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Metadata

makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Record
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Stations
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Station
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Connection
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Report
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Location
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Program
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName (mkName $ nameBase name)])) ''Operator
