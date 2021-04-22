{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Types where
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word

newtype DatasetName = DatasetName { fromDatasetName :: Text }

data Config = Config
  { teamWritekey :: Text
  , defaultDataset :: DatasetName
  , apiHost :: Text
  , sampleRate :: Maybe Word64
  , pendingQueueSize :: Word64
  , responseQueueSize :: Word64
  , sendThreads :: Word64
  , sendBlocking :: Bool
  , nullTransmission :: Bool
  , customUserAgent :: ByteString
  }

config :: Text -> DatasetName -> Config
config k ds = Config k ds "api.honeycomb.io" Nothing 0 0 0 False False ""
