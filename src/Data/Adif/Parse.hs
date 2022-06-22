module Data.Adif.Parse where

import Prelude()
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.String (String)
import Control.Monad (return, (>>))
import Data.Function (($))
import Text.Read (read)


-- | Parses ADI Data-Specifiers
-- According to the https://www.adif.org/313/ADIF_313.htm#ADI_Data_Specifiers
-- Data type indicator T and data length are omitted.
parseField :: Parser Field
parseField = do
  _ <- char '<'
  name <- many1 $ letter <|> char '_'
  _ <- char ':'
  length <- many1 digit
  _ <- optional $ char ':' >> many letter
  _ <- char '>'
  value <- count (read length) anyChar
  return $ Field name value

data Field = Field FieldName FieldData
type FieldName = String
type FieldData = String
