module Data.Adif.Model
  ( Document (..),
    Header (),
    Record (..),
  )
where

import Data.Adif.Definition (headerFields, recordFields)
import Data.Adif.TH (mkFieldName)
import Data.Char (toLower)
import Data.Empty (Empty)
import GHC.Generics (Generic)
import Language.Haskell.TH (Bang (Bang), Body (NormalB), Clause (Clause), Con (RecC), Dec (DataD, FunD, SigD), DerivClause (DerivClause), Exp (AppE, ConE), SourceStrictness (NoSourceStrictness), SourceUnpackedness (NoSourceUnpackedness), Type (AppT, ConT), mkName)

$( pure
     [ DataD
         []
         (mkName "Header")
         []
         Nothing
         [ RecC
             (mkName "Header")
             ( (mkName "text", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''String)
                 : [ ( mkFieldName record,
                       Bang NoSourceUnpackedness NoSourceStrictness,
                       AppT
                         (ConT ''Maybe)
                         (ConT ''String)
                     )
                     | record <- headerFields
                   ]
             )
         ]
         [ DerivClause
             Nothing
             [ ConT name
               | name <- [''Eq, ''Show, ''Read, ''Generic, ''Empty]
             ]
         ],
       DataD
         []
         (mkName "Record")
         []
         Nothing
         [ RecC
             (mkName "Record")
             [ ( mkFieldName record,
                 Bang NoSourceUnpackedness NoSourceStrictness,
                 AppT
                   (ConT ''Maybe)
                   (ConT ''String)
               )
               | record <- recordFields
             ]
         ]
         [ DerivClause
             Nothing
             [ ConT name
               | name <- [''Eq, ''Show, ''Read, ''Generic, ''Empty]
             ]
         ]
     ]
 )

data Document = Document
  { header :: Maybe Header,
    records :: [Record]
  }
  deriving (Eq, Show, Read, Generic, Empty)
