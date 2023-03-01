module Data.Adif.Builder (toAdif) where

import Data.Adif (Record (..))
import Data.Adif.Definition (qsoFields)
import Data.Char (toLower)
import Language.Haskell.TH.Syntax (Exp (AppE, ListE, LitE, TupE, VarE), Lit (StringL), mkName)

toAdif :: [Record] -> String
toAdif records = concatMap (toAdifRecord . fields) records <> "\n"
  where
    fields record =
      $( pure $
           ListE
             [ TupE
                 [ Just $ LitE $ StringL name,
                   Just $ AppE (VarE $ mkName ("_" <> (toLower <$> name))) (VarE 'record)
                 ]
               | name <- qsoFields
             ]
       )

toAdifRecord :: [(String, Maybe String)] -> String
toAdifRecord fields = concatMap toAdifField fields <> "<eor>" <> "\n"

toAdifField :: (String, Maybe String) -> String
toAdifField (key, Just val) = "<" <> key <> ":" <> show (length val) <> ">" <> val <> " "
toAdifField (_, Nothing) = mempty
