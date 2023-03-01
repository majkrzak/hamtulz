module Data.Adif (Record (..), emptyRecord) where

import Data.Adif.Definition (qsoFields)
import Data.Char (toLower)
import GHC.Generics (Generic)
import Language.Haskell.TH (Bang (Bang), Body (NormalB), Clause (Clause), Con (RecC), Dec (DataD, FunD, SigD), DerivClause (DerivClause), Exp (AppE, ConE), SourceStrictness (NoSourceStrictness), SourceUnpackedness (NoSourceUnpackedness), Type (AppT, ConT), mkName)

$( pure
     [ DataD
         []
         (mkName "Record")
         []
         Nothing
         [ RecC
             (mkName "Record")
             [ ( mkName ("_" <> (toLower <$> record)),
                 Bang NoSourceUnpackedness NoSourceStrictness,
                 AppT
                   (ConT ''Maybe)
                   (ConT ''String)
               )
               | record <- qsoFields
             ]
         ]
         [ DerivClause
             Nothing
             [ ConT name
               | name <- [''Eq, ''Show, ''Read, ''Generic]
             ]
         ],
       SigD
         (mkName "emptyRecord")
         (ConT $ mkName "Record"),
       FunD
         (mkName "emptyRecord")
         [ Clause
             []
             ( NormalB $
                 foldl (\x _ -> AppE x (ConE 'Nothing)) (ConE $ mkName "Record") qsoFields
             )
             []
         ]
     ]
 )
