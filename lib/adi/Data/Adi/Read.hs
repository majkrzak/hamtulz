module Data.Adi.Read () where

import Control.Monad (void)
import Data.Adi.Model (Document (Document), Field (Field), Header (Header), Record (Record))
import Data.Char (isAscii, isDigit, isLetter, ord, toUpper)
import Text.ParserCombinators.ReadP (ReadP, between, char, count, eof, get, look, many, many1, manyTill, munch, munch1, satisfy, sepBy, skipSpaces, (+++))
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
field = trim $ do
  '<' <- char '<'
  name <- map toUpper <$> munch1 (\c -> (isAscii c && isLetter c) || c == '_')
  ':' <- char ':'
  len <- read <$> munch1 isDigit
  '>' <- char '>'
  payload <- count len get
  return $ Field (name, payload)

fields :: ReadP [Field]
fields = trim $ many field

record :: ReadP Record
record = trim $ do
  fields' <- fields
  eor
  return $ Record fields'

records :: ReadP [Record]
records = trim $ many record

header :: ReadP Header
header = trim $ do
  text <- trim string1
  fields' <- fields
  eoh
  return $ Header (text, fields')

maybeHeader :: ReadP (Maybe Header)
maybeHeader =
  look >>= \case
    [] -> return Nothing
    ('<' : _) -> return Nothing
    _ -> Just <$> header

document :: ReadP Document
document = trim $ do
  maybeHeader' <- maybeHeader
  records' <- records
  eof
  return $ Document (maybeHeader', records')

eor :: ReadP ()
eor = trim $ do
  '<' <- char '<'
  'E' <- toUpper <$> (char 'e' +++ char 'E')
  'O' <- toUpper <$> (char 'o' +++ char 'O')
  'R' <- toUpper <$> (char 'r' +++ char 'R')
  '>' <- char '>'
  return ()

eoh :: ReadP ()
eoh = trim $ do
  '<' <- char '<'
  'E' <- toUpper <$> (char 'e' +++ char 'E')
  'O' <- toUpper <$> (char 'o' +++ char 'O')
  'H' <- toUpper <$> (char 'h' +++ char 'H')
  '>' <- char '>'
  return ()

trim :: ReadP a -> ReadP a
trim = between skipSpaces skipSpaces

-- an ASCII character whose code lies in the range of 32 through 126, inclusive
character :: ReadP Char
character = satisfy (\c -> 32 <= ord c && ord c <= 126)

-- a sequence of Characters
string1 :: ReadP String
string1 = many1 character
