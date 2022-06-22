module Data.Adif.Parse where

import Prelude()
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.String (String)
import Control.Monad (return, (>>), void)
import Data.Function (($))
import Text.Read (read)
import Text.Show (Show)
import Data.Char (toLower)
import Control.Applicative ((<$>))


-- | Parses ADI Data-Specifiers
-- According to the https://www.adif.org/313/ADIF_313.htm#ADI_Data_Specifiers
-- Data type indicator T and data length are omitted.
field :: Parser Field
field = try $ do
  void $ char '<'
  name <- many1 $ letter <|> char '_'
  void $ char ':'
  length <- many1 digit
  void $ optional $ char ':' >> many letter
  void $ char '>'
  value <- count (read length) anyChar
  spaces
  return $ Field (toLower <$> name, value)

-- | Parses ADI Records
-- According to the https://www.adif.org/313/ADIF_313.htm#ADI_Record
record :: Parser Record
record = try $ do
  fields <- many field
  eor
  return $ Record fields

-- | Parses ADI Header
header :: Parser Header
header = try $ do
  text <- manyTill anyChar $ lookAhead (choice [void field, eoh])
  fields <- many field
  eoh
  return $ Header (text,fields)

-- | Parses End-Of-Header tag
eoh :: Parser ()
eoh = try $ char '<' >> oneOf "Ee" >> oneOf "Oo" >> oneOf "Hh" >> char '>' >> spaces

-- | Parses End-Of-Record tag
eor :: Parser ()
eor = try $ char '<' >> oneOf "Ee" >> oneOf "Oo" >> oneOf "Rr" >> char '>' >> spaces


newtype Header = Header (String, [Field]) deriving Show
newtype Record = Record [Field] deriving Show
newtype Field = Field (FieldName, FieldData) deriving Show
type FieldName = String
type FieldData = String
