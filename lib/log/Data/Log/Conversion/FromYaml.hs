{-# LANGUAGE RecordWildCards #-}

module Data.Log.Conversion.FromYaml () where

import Data.Log.Model (Connection (..), Contacted (..), Document (..), Location (..), Logging (..), Metadata (..), Operator (..), Program (..), Record (..), Report (..), Station (..), Stations (..), Via (..))
import Data.Map (fromList)
import Data.Radio (Band, Locator, Mode)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import Data.Yaml.Parser (FromYaml, fromYaml, withMapping)

instance FromYaml Document where
  fromYaml = withMapping "Document" $ \k -> do
    metadata :: Maybe Metadata <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "metadata" k)
    contacts :: Maybe [Record] <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "contacts" k)
    return $ Document {..}

instance FromYaml Metadata where
  fromYaml = withMapping "Metadata" $ \k -> do
    callsigns :: Maybe [Text] <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "callsigns" k)
    locations :: Maybe [Location] <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "locations" k)
    loggings :: Maybe [Logging] <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "loggings" k)
    return $ Metadata {..}

instance FromYaml Record where
  fromYaml = withMapping "Record" $ \k -> do
    datetime :: UTCTime <- maybe (fail "datetime missing") (fromYaml) (lookup "datetime" k)
    stations :: Maybe Stations <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "stations" k)
    connection :: Maybe Connection <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "connection" k)
    report :: Maybe Report <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "report" k)
    return $ Record {..}

instance FromYaml Stations where
  fromYaml = withMapping "Stations" $ \k -> do
    logging :: Maybe Logging <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "logging" k)
    contacted :: Maybe Contacted <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "contacted" k)
    return $ Stations {..}

instance FromYaml Logging where
  fromYaml = fmap Logging . fromYaml

instance FromYaml Contacted where
  fromYaml = fmap Contacted . fromYaml

instance FromYaml Station where
  fromYaml = withMapping "Station" $ \k -> do
    callsign :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "callsign" k)
    operator :: Maybe Operator <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "operator" k)
    location :: Maybe Location <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "location" k)
    return $ Station {..}

instance FromYaml Connection where
  fromYaml = withMapping "Connection" $ \k -> do
    band :: Maybe Band <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "band" k)
    band_rx :: Maybe Band <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "band_rx" k)
    mode :: Maybe Mode <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "mode" k)
    frequency :: Maybe Double <- maybe (return Nothing) (fmap (Just . read . unpack) . fromYaml) (lookup "frequency" k)
    frequency_rx :: Maybe Double <- maybe (return Nothing) (fmap (Just . read . unpack) . fromYaml) (lookup "frequency_rx" k)
    via :: Maybe Via <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "via" k)
    return $ Connection {..}

instance FromYaml Via where
  fromYaml = withMapping "Via" $ \k -> do
    satellite :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "satellite" k)
    repeater :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "repeater" k)
    talkgroup :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "talkgroup" k)
    return $ Via {..}

instance FromYaml Location where
  fromYaml = withMapping "Program" $ \k -> do
    dxcc :: Maybe Int <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "dxcc" k)
    gridsquare :: Maybe Locator <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "gridsquare" k)
    description :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "description" k)
    program :: Maybe Program <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "program" k)
    return $ Location {..}

instance FromYaml Program where
  fromYaml = withMapping "Program" $ \k -> do
    pga :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "pga" k)
    sota :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "sota" k)
    pota :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "pota" k)
    iota :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "iota" k)
    wwff :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "wwff" k)
    wca :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "wca" k)
    return $ Program {..}

instance FromYaml Operator where
  fromYaml = withMapping "Operator" $ \k -> do
    name :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "name" k)
    age :: Maybe Int <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "age" k)
    return $ Operator {..}

instance FromYaml Report where
  fromYaml = withMapping "Report" $ \k -> do
    sent :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "sent" k)
    rcvd :: Maybe Text <- maybe (return Nothing) (fmap Just . fromYaml) (lookup "rcvd" k)
    return $ Report {..}