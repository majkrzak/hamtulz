{-# LANGUAGE TemplateHaskell #-}

module Data.Adif where


import GHC.Generics (Generic)
import Data.Char (toLower)
import Data.Adif.Definition (qsoFields)
import Language.Haskell.TH (Dec(DataD,FunD,SigD),Lit(StringL),Clause(Clause),Body(NormalB),Exp(LitE,AppE,ConE) ,Con(RecC), DerivClause(DerivClause), mkName, Bang(Bang), SourceUnpackedness(NoSourceUnpackedness),SourceStrictness(NoSourceStrictness), Type(ConT) )

$(pure [
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
  ,SigD (mkName "emptyRecord") (ConT $ mkName "Record")
  ,FunD
    (mkName "emptyRecord")
    [
      Clause
        []
        (
          NormalB $
            foldl (\x _ -> AppE x (LitE $ StringL "")) (ConE $ mkName "Record") qsoFields
        )
        []
    ]
 ])
