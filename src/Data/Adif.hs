{-# LANGUAGE TemplateHaskell #-}

module Data.Adif where


import GHC.Generics (Generic)
import Data.Char (toLower)
import Data.Adif.Definition (qsoFields)
import Language.Haskell.TH (Dec(DataD), Con(RecC), DerivClause(DerivClause), mkName, Bang(Bang), SourceUnpackedness(NoSourceUnpackedness),SourceStrictness(NoSourceStrictness), Type(ConT) )

$(return [
  DataD
    []
    (mkName "Record")
    []
    Nothing
    [
      RecC
        (mkName "Record")
        [
          (mkName ("_" <> (toLower <$> record)),Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName "String"))
          | record <- qsoFields
        ]
    ]
    [ DerivClause Nothing
      [
        ConT (mkName name) 
        | name <- ["Eq", "Show", "Read", "Generic"]
      ]
    ] 
 ])
