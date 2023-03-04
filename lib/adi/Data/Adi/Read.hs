module Data.Adi.Read () where

import Control.Monad (void)
import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))
import Data.Char (isDigit, isLetter, toUpper)
import Text.ParserCombinators.ReadP (ReadP, char, count, eof, get, look, many, manyTill, munch1, sepBy, skipSpaces, (+++))
import Text.ParserCombinators.ReadPrec (readPrec_to_P)
import Text.Read (lift, readPrec)

instance Read Field where
  readPrec = lift field

instance {-# OVERLAPPING #-} Read [Field] where
  readPrec = lift fields

instance Read Record where
  readPrec = lift record

instance {-# OVERLAPPING #-} Read [Record] where
  readPrec = lift records

instance Read Header where
  readPrec = lift header

instance Read Document where
  readPrec = lift document

field :: ReadP Field
field = do
  '<' <- char '<'
  name <- map toUpper <$> munch1 (\c -> isLetter c || c == '_')
  ':' <- char ':'
  len <- read <$> munch1 isDigit
  '>' <- char '>'
  payload <- count len get
  skipSpaces
  return $ Field (name, payload)

fields :: ReadP [Field]
fields = many field

record :: ReadP Record
record = do
  fields' <- fields
  eor
  skipSpaces
  return $ Record fields'

records :: ReadP [Record]
records = many record

header :: ReadP Header
header = do
  text <- manyTill get (eoh +++ void field)
  fields' <- fields
  eoh
  skipSpaces
  return $ Header (text, fields')

maybeHeader :: ReadP (Maybe Header)
maybeHeader =
  look >>= \case
    ('<' : _) -> return Nothing
    _ -> Just <$> header

document :: ReadP Document
document = do
  maybeHeader' <- maybeHeader
  records' <- records
  eof
  return $ Document (maybeHeader', records')

eor :: ReadP ()
eor = do
  '<' <- char '<'
  'E' <- toUpper <$> char 'e' +++ char 'E'
  'O' <- toUpper <$> char 'o' +++ char 'O'
  'F' <- toUpper <$> char 'f' +++ char 'F'
  return ()

eoh :: ReadP ()
eoh = do
  '<' <- char '<'
  'E' <- toUpper <$> char 'e' +++ char 'E'
  'O' <- toUpper <$> char 'o' +++ char 'O'
  'H' <- toUpper <$> char 'h' +++ char 'H'
  return ()
