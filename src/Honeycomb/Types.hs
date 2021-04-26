{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Types where
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word
import Lens.Micro
import Data.String (IsString)

newtype DatasetName = DatasetName { fromDatasetName :: Text }
  deriving (Show, Eq, Ord, IsString)

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

class HasConfig a where
  configL :: Lens' a Config

instance HasConfig Config where
  configL = lens id (\_ new -> new)

-- | Smart constructor with sane defaults for Honeycomb config options.
--
-- @since 0.0.1
config :: Text -> DatasetName -> Config
config k ds = Config k ds "api.honeycomb.io" Nothing 0 0 0 False False ""
