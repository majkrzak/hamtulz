{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Adif.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Adif
import Control.Monad (void)
import Data.Char (toUpper, toLower)
import Language.Haskell.TH (Q, Exp(ListE,TupE,LitE,LamE,RecUpdE,VarE), Lit(StringL), Pat(VarP), mkName)
import Data.Adif.Definition (qsoFields)


parseRecord :: Parser Record
parseRecord = try $ do
  fields <- many (choice $ map (uncurry parseField ) $(pure $ ListE [TupE [Just $ LitE $ StringL name, Just $ LamE [VarP $ mkName "x",VarP $ mkName "r"] $ RecUpdE (VarE $ mkName "r") [(mkName ("_" <> (toLower <$> name)), VarE $ mkName "x")] ]  |name <- qsoFields]))
  eor
  return $ foldl (\r f -> f r) emptyRecord fields

parseField :: String -> (String -> Record -> Record) -> Parser (Record -> Record)
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

-- | Parses End-Of-Header tag
eoh :: Parser ()
eoh = try $ char '<' >> oneOf "Ee" >> oneOf "Oo" >> oneOf "Hh" >> char '>' >> spaces

-- | Parses End-Of-Record tag
eor :: Parser ()
eor = try $ char '<' >> oneOf "Ee" >> oneOf "Oo" >> oneOf "Rr" >> char '>' >> spaces


caseChar :: Stream s m Char => Char -> ParsecT s u m Char
caseChar c = char (toLower c) <|> char (toUpper c)

caseString :: Stream s m Char => String -> ParsecT s u m ()
caseString cs = mapM_ caseChar cs <?> cs