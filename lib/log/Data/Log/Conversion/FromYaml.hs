{-# LANGUAGE RecordWildCards #-}

module Data.Log.Conversion.FromYaml () where

import Control.Lens (at, (^.))
import Data.Log.Model (Connection (..), Contacted (..), Document (..), Location (..), Logging (..), Metadata (..), Operator (..), Program (..), Record (..), Report (..), Station (..), Stations (..))
import Data.Map (fromList)
import Data.Radio (Band, Mode)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import Data.Yaml.Parser (FromYaml, fromYaml, withMapping)

instance FromYaml Document where
  fromYaml = withMapping "Document" $ \k -> do
    metadata :: Maybe Metadata <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "metadata")
    contacts :: Maybe [Record] <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "contacts")
    return $ Document {..}

instance FromYaml Metadata where
  fromYaml = withMapping "Metadata" $ \k -> do
    callsigns :: Maybe [Text] <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "callsigns")
    locations :: Maybe [Location] <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "locations")
    loggings :: Maybe [Logging] <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "loggings")
    return $ Metadata {..}

instance FromYaml Record where
  fromYaml = withMapping "Record" $ \k -> do
    datetime :: UTCTime <- maybe (fail "datetime missing") (fromYaml) (fromList k ^. at "datetime")
    stations :: Maybe Stations <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "stations")
    connection :: Maybe Connection <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "connection")
    report :: Maybe Report <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "report")
    return $ Record {..}

instance FromYaml Stations where
  fromYaml = withMapping "Stations" $ \k -> do
    logging :: Maybe Logging <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "logging")
    contacted :: Maybe Contacted <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "contacted")
    return $ Stations {..}

instance FromYaml Logging where
  fromYaml = fmap Logging . fromYaml

instance FromYaml Contacted where
  fromYaml = fmap Contacted . fromYaml

instance FromYaml Station where
  fromYaml = withMapping "Station" $ \k -> do
    callsign :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "callsign")
    operator :: Maybe Operator <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "operator")
    location :: Maybe Location <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "location")
    return $ Station {..}

instance FromYaml Connection where
  fromYaml = withMapping "Connection" $ \k -> do
    band :: Maybe Band <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "band")
    band_rx :: Maybe Band <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "band_rx")
    mode :: Maybe Mode <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "mode")
    frequency :: Maybe Double <- maybe (return Nothing) (fmap (Just . read . unpack) . fromYaml) (fromList k ^. at "frequency")
    frequency_rx :: Maybe Double <- maybe (return Nothing) (fmap (Just . read . unpack) . fromYaml) (fromList k ^. at "frequency_rx")
    return $ Connection {..}

instance FromYaml Location where
  fromYaml = withMapping "Program" $ \k -> do
    dxcc :: Maybe Int <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "dxcc")
    gridsquare :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "gridsquare")
    description :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "description")
    program :: Maybe Program <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "program")
    return $ Location {..}

instance FromYaml Program where
  fromYaml = withMapping "Program" $ \k -> do
    pga :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "pga")
    sota :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "sota")
    pota :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "pota")
    iota :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "iota")
    wwff :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "wwff")
    wca :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "wca")
    return $ Program {..}

instance FromYaml Operator where
  fromYaml = withMapping "Operator" $ \k -> do
    name :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "name")
    age :: Maybe Int <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "age")
    return $ Operator {..}

instance FromYaml Report where
  fromYaml = withMapping "Report" $ \k -> do
    sent :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "sent")
    rcvd :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (fromList k ^. at "rcvd")
    return $ Report {..}