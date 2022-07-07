module Main where

import qualified Data.Adif.Parser as AdifParser
import Data.Adif2Log (adif2log)
import qualified Data.ByteString.Char8 as BS8
import Data.Yaml.Builder (toByteString)
import Data.Yaml.Generic ()
import Data.Yaml.Instances ()
import Text.Parsec.String (parseFromFile)
import System.Environment.XDG.BaseDir (getUserDataFile)

main :: IO ()
main = do
  adifFile <- getUserDataFile "WSJT-X" "wsjtx_log.adi"
  adif <- parseFromFile AdifParser.file adifFile
      >>= \case
            Left  err -> fail $ show err
            Right xs  -> return xs
  BS8.putStrLn $ toByteString $ adif2log adif
