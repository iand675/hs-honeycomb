module Honeycomb.Config where
import Honeycomb.Types
import Data.Text (Text)
import Lens.Micro (Lens', lens)
import Data.ByteString (ByteString)
import Data.Word (Word64)

data Config = Config
  { teamWritekey :: Text
  , defaultDataset :: DatasetName
  , apiHost :: Text
  , sampleRate :: Maybe Word64
  , pendingQueueSize :: Word64
  -- , responseQueueSize :: Word64
  , sendThreads :: Word64
  , sendBlocking :: Bool
  , nullTransmission :: Bool -- TODO
  , customUserAgent :: ByteString -- TODO
  }

class HasConfig a where
  configL :: Lens' a Config

instance HasConfig Config where
  configL = lens id (\_ new -> new)

-- | Smart constructor with sane defaults for Honeycomb config options.
--
-- To alter options, import @Honeycomb.Config@
--
-- > import qualified Honeycomb.Config as Config
-- > config { Config.pendingQueueSize = 512 }
--
-- @since 0.0.1
config :: Text -> DatasetName -> Config
config k ds = Config k ds "api.honeycomb.io" Nothing 1024 1 False False ""
