module Data.Adif.Parser (file, record) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Adif
import Control.Monad (void)
import Data.Char (toUpper, toLower)
import Language.Haskell.TH (Exp(ListE,TupE,LitE,LamE,RecUpdE,VarE,ConE,AppE), Lit(StringL), Pat(VarP), mkName)
import Data.Adif.Definition (qsoFields)

-- | Parses ADI File
-- According to the https://www.adif.org/313/ADIF_313.htm#ADI_File_Format
-- Return list of Records
file :: Parser [Record]
file = try $ do
  _       <- optionMaybe header
  records <- many record
  eof
  return records

-- | Parses ADI Header
-- Dummy implementation which ignore the content.
header :: Parser ()
header = try $ void $ manyTill anyChar eoh

-- | Parses ADI Records
-- According to the https://www.adif.org/313/ADIF_313.htm#ADI_Record
-- User defined fields are not supported yet.
record :: Parser Record
record = try $ do
  fields <- many
    (choice $ map
      (uncurry field)
      $(pure $
        ListE
          [ TupE
            [ Just $ LitE $ StringL name
            , Just $ LamE
              [ VarP $ mkName "x",VarP $ mkName "r"
              ] $ RecUpdE (VarE $ mkName "r")
                [ ( mkName ("_" <> (toLower <$> name))
                  , AppE
                    (ConE 'Just)
                    (VarE $ mkName "x")
                  )
                ]
             ] |name <- qsoFields
          ]
       )
    )
  eor
  return $ foldl (\r f -> f r) emptyRecord fields

-- | Parses ADI Data-Specifiers
-- According to the https://www.adif.org/313/ADIF_313.htm#ADI_Data_Specifiers
-- For given field name and record setter return parser with
-- setter application over a record.
field :: String -> (String -> Record -> Record) -> Parser (Record -> Record)
field name set = try $ do
  void $ char '<'
  void $ identifier name
  void $ char ':'
  len <- many1 digit
  void $ optional $ char ':' >> many letter
  void $ char '>'
  val <- count (read len) anyChar
  spaces
  return $ set val

-- | Parses End-Of-Header tag
eoh :: Parser ()
eoh = try $ char '<' >> identifier "EOH" >> char '>' >> spaces

-- | Parses End-Of-Record tag
eor :: Parser ()
eor = try $ char '<' >> identifier "EOR" >> char '>' >> spaces

-- | Parses case insensitive identifier
identifier :: Stream s m Char => String -> ParsecT s u m String
identifier = mapM (\c -> char (toLower c) <|> char (toUpper c))
