{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Adif.Parser where

import           Control.Monad                  ( void )
import           Data.Adif
import           Data.Adif.Definition           ( qsoFields )
import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Language.Haskell.TH            ( Exp
                                                  ( LamE
                                                  , ListE
                                                  , LitE
                                                  , RecUpdE
                                                  , TupE
                                                  , VarE
                                                  )
                                                , Lit(StringL)
                                                , Pat(VarP)
                                                , Q
                                                , mkName
                                                )
import           Text.Parsec
import           Text.Parsec.String             ( Parser )


file :: Parser [Record]
file = try $ do
  _       <- header
  records <- many parseRecord
  eof
  return records

parseRecord :: Parser Record
parseRecord = try $ do
  fields <- many
    (choice $ map
      (uncurry parseField)
      $(pure $
        ListE
          [ TupE
            [ Just $ LitE $ StringL name
            , Just $ LamE
              [ VarP $ mkName "x",VarP $ mkName "r"
              ] $ RecUpdE (VarE $ mkName "r")
                [ (mkName ("_" <> (toLower <$> name)), VarE $ mkName "x")
                ]
             ] |name <- qsoFields
          ]
       )
    )
  eor
  return $ foldl (\r f -> f r) emptyRecord fields

parseField
  :: String -> (String -> Record -> Record) -> Parser (Record -> Record)
parseField name set = try $ do
  void $ char '<'
  void $ caseString name
  void $ char ':'
  len <- many1 digit
  void $ optional $ char ':' >> many letter
  void $ char '>'
  val <- count (read len) anyChar
  spaces
  return $ set val

header :: Parser ()
header =
  try $ void (lookAhead $ try $ char '<') <|> void (manyTill anyChar eoh)


-- | Parses End-Of-Header tag
eoh :: Parser ()
eoh =
  try $ char '<' >> oneOf "Ee" >> oneOf "Oo" >> oneOf "Hh" >> char '>' >> spaces

-- | Parses End-Of-Record tag
eor :: Parser ()
eor =
  try $ char '<' >> oneOf "Ee" >> oneOf "Oo" >> oneOf "Rr" >> char '>' >> spaces


caseChar :: Stream s m Char => Char -> ParsecT s u m Char
caseChar c = char (toLower c) <|> char (toUpper c)

caseString :: Stream s m Char => String -> ParsecT s u m ()
caseString cs = mapM_ caseChar cs <?> cs
