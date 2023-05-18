module Main where

import Data.Log qualified as Log
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Valid (validate, validator)
import System.Environment (getArgs)
import Data.Yaml.Parser (readYamlFile)

main :: IO ()
main = do
  [logFile] <- getArgs
  document :: Log.Document <- readYamlFile logFile
  putStrLn $ unlines $ validate validator document
  mempty
