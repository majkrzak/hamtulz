module Data.Adif.Conversion.Adi (toAdi, fromAdi) where

import Data.Adif.Model (Document, Header, Record)
import Data.Adif.Lens
import Data.Adi qualified as Adi
import Data.Adif.TH (mkFieldName)
import Data.Empty (empty, Empty)
import Data.Maybe (catMaybes)
import Data.Char (toLower)
import Language.Haskell.TH (Exp(ListE, AppE,VarE,LitE, TupE), Lit(StringL), mkName)
import Data.Adif.Definition (headerFields, recordFields)
import Data.Foldable (find)


import Control.Lens (set, view, ALens)

toAdi :: Document ->  Adi.Document
toAdi document' = ((set Adi.header (headerToAdi <$> view header document')) . (set Adi.records (recordToAdi <$>  view records document'))) empty

recordToAdi :: Record -> Adi.Record
recordToAdi record' = set Adi.fields (catMaybes
        $(pure $
            ListE [
                AppE
                    (AppE
                      (VarE 'fieldToAdi)
                      ( LitE (StringL recordField))
                    )
                    ( AppE
                               (AppE
                                  (VarE 'view)
                                  (VarE $ mkFieldName recordField)
                               )
                               (VarE 'record')
                           )
            | recordField <- recordFields ]
        )
        -- [e|fieldToAdi "ADDRESS" (view _address record')|]
    ) empty

headerToAdi :: Header -> Adi.Header
headerToAdi header' = (set Adi.text (view text header') . (
        set Adi.fields (catMaybes
            $(pure $
                ListE [
                    AppE
                        (AppE
                        (VarE 'fieldToAdi)
                        ( LitE (StringL headerField))
                        )
                        ( AppE
                                (AppE
                                    (VarE 'view)
                                    (VarE $ mkFieldName headerField)
                                )
                                (VarE 'header')
                            )
                | headerField <- headerFields ]
            )
        )
    )) empty

fieldToAdi :: String -> Maybe String -> Maybe Adi.Field
fieldToAdi name' maybePayload' = case maybePayload' of
                                    Just payload' -> Just (((set Adi.name name') . (set Adi.payload payload')) empty)
                                    Nothing -> Nothing



fromAdi :: Adi.Document -> Document
fromAdi document' =
    apply [
        set header (headerFromAdi <$> view Adi.header document'),
        set records (recordFromAdi <$> view Adi.records document')
    ] empty

headerFromAdi :: Adi.Header -> Header
headerFromAdi header' =
    apply ([
        set text (view Adi.text header')
    ] <> $(
        do
            expressions <- sequence [
                [|
                    set
                        $( pure $ VarE $ mkFieldName headerField )
                        ( (view Adi.payload) <$> ( find (\field' -> view Adi.name field' == $(pure $ LitE $ StringL headerField) ) (view Adi.fields header')))
                |]
              | headerField <- headerFields
              ]
            return $ ListE expressions
    )) empty

recordFromAdi :: Adi.Record -> Record
recordFromAdi record' =
    apply ($(
        do
            expressions <- sequence [
                [|
                    set
                        $( pure $ VarE $ mkFieldName recordField )
                        ( (view Adi.payload) <$> ( find (\field' -> view Adi.name field' == $(pure $ LitE $ StringL recordField) ) (view Adi.fields record')))
                |]
              | recordField <- recordFields
              ]
            return $ ListE expressions
    )) empty

apply :: Empty a => [a -> a] -> a -> a
apply = foldl (.) id
