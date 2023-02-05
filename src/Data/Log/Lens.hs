module Data.Log.Lens where

import Control.Lens (makeLensesWith, lensRules, lensField, (&), (.~), DefName(TopName))
import qualified Data.Log as Log
import Language.Haskell.TH (mkName, nameBase)

makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Record
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Stations
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Station
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Connection
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Band 
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Mode
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Report
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Location
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Program 
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Antenna
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.AntennaKind
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.AntennaOrientation
makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName ( mkName $ nameBase name )])) ''Log.Operator
