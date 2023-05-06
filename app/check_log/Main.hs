module Main where

import Data.Log qualified as Log
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Valid (validate, validator)
import Data.Yaml (decodeFileWithWarnings)
import Data.Yaml.AesonInstances ()
import System.Environment (getArgs)

main :: IO ()
main = do
  [logFile] <- getArgs
  (warning, document :: Log.Document) <-
    decodeFileWithWarnings logFile
      >>= \case
        Left err -> fail $ show err
        Right xs -> return xs
  print warning
  putStrLn $ unlines $ validate validator document
  mempty
