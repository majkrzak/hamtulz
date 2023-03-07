module Data.Adif.Model (Document(..), Header(), Record(..)) where

import Data.Adif.Definition (headerFields, recordFields)
import Data.Char (toLower)
import Data.Empty (Empty)
import GHC.Generics (Generic)
import Language.Haskell.TH (Bang (Bang), Body (NormalB), Clause (Clause), Con (RecC), Dec (DataD, FunD, SigD), DerivClause (DerivClause), Exp (AppE, ConE), SourceStrictness (NoSourceStrictness), SourceUnpackedness (NoSourceUnpackedness), Type (AppT, ConT), mkName)

$( pure
     [
        DataD
         []
         (mkName "Header")
         []
         Nothing
         [ RecC
             (mkName "Header")
             [ ( mkName ("_" <> (toLower <$> record)),
                 Bang NoSourceUnpackedness NoSourceStrictness,
                 AppT
                   (ConT ''Maybe)
                   (ConT ''String)
               )
               | record <- headerFields
             ]
         ]
         [ DerivClause
             Nothing
             [ ConT name
               | name <- [''Eq, ''Show, ''Read, ''Generic, ''Empty]
             ]
         ]
     , DataD
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

data Document = Document {
    _header :: Maybe Header,
    _records :: [Record]
} deriving (Eq, Show, Read, Generic, Empty)
