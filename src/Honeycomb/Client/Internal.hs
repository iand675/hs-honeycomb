{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Honeycomb.Client.Internal where

import Chronos
import Control.Concurrent.Async
import Data.Aeson (Value, ToJSON, FromJSON, eitherDecode, encode)
import Data.HashMap.Strict as S
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Client
import System.Random.MWC
import Honeycomb.Types
import Data.Vector (Vector)
import Data.RingBuffer
import Network.HTTP.Types
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

data HoneycombClient = HoneycombClient
  { clientManager :: Manager
  , clientConfig :: Config
  , clientGen :: GenIO
  -- | Subject to change
  -- TODO this needs to respect dispatching to custom host/dataset/writekey/etc.
  , clientEventBuffer :: RingBuffer 'MultiWriter Vector Event
  -- , clientQueueMap :: Map ThreadId 
  , clientWorker :: Async ()
  }

data Event = Event
  { _fields :: S.HashMap Text Value
  , _teamWriteKey :: Maybe Text
  , _dataset :: Maybe DatasetName
  , _apiHost :: Maybe Text
  , _sampleRate :: Maybe Word64
  , _timestamp :: Maybe Time
  }


post :: (ToJSON a) => (Request -> Manager -> IO (Response b)) -> HoneycombClient -> [Text] -> RequestHeaders -> a -> IO (Response b)
post f HoneycombClient{..} pathPieces hs x = do
  f req clientManager
  where 
    req = defaultRequest 
        { method = methodPost
        , host = "api.honeycomb.io"
        , path = T.encodeUtf8 $ T.intercalate "/" pathPieces
        , requestHeaders = hs ++
            [ (hUserAgent, "libhoneycomb-haskell/0.1")
            , (hContentType, "application/json")
            , ("X-Honeycomb-Team", T.encodeUtf8 $ teamWritekey clientConfig)
            ]
        -- TODO
        , requestBody = RequestBodyLBS $ encode x
        }

decodeJSON :: FromJSON a => Response L.ByteString -> Response (Either String a)
decodeJSON = fmap eitherDecode