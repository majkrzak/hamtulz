module Data.Adif (Record(..), emptyRecord) where

import GHC.Generics (Generic)
import Data.Char (toLower)
import Data.Adif.Definition (qsoFields)
import Language.Haskell.TH (Dec(DataD,FunD,SigD),Clause(Clause),Body(NormalB),Exp(AppE,ConE),Con(RecC),DerivClause(DerivClause),mkName,Bang(Bang),SourceUnpackedness(NoSourceUnpackedness),SourceStrictness(NoSourceStrictness),Type(AppT,ConT) )


$(pure
  [ DataD
    []
    (mkName "Record")
    []
    Nothing
    [ RecC
      (mkName "Record")
      [ ( mkName ("_" <> (toLower <$> record))
        , Bang NoSourceUnpackedness NoSourceStrictness
        , AppT
          (ConT ''Maybe)
          (ConT ''String)
        )
        | record <- qsoFields
      ]
    ]
    [ DerivClause Nothing
      [ ConT name
        | name <- [''Eq, ''Show, ''Read, ''Generic]
      ]
    ]
  , SigD
    (mkName "emptyRecord")
    (ConT $ mkName "Record")
  , FunD
    (mkName "emptyRecord")
    [ Clause
      []
      ( NormalB $
        foldl (\x _ -> AppE x (ConE 'Nothing)) (ConE $ mkName "Record") qsoFields
      )
      []
    ]
  ])
