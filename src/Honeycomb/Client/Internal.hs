{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
import Network.HTTP.Types
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class
import UnliftIO.STM (TBQueue)

data HoneycombClient = HoneycombClient
  { clientConfig :: Config
  , clientGen :: GenIO
  -- | Subject to change
  -- TODO this respects dispatching to custom host/dataset/writekey/etc, but needs a means of
  -- using the bulk events API instead of dispatching a bunch of single event calls.
  , clientEventBuffer :: TBQueue (IO ())
  -- , clientQueueMap :: Map ThreadId 
  , clientWorkers :: [Async ()]
  }

class HasHoneycombClient a where
  honeycombClientL :: Lens' a HoneycombClient

instance HasHoneycombClient HoneycombClient where
  honeycombClientL = lens id (\_ new -> new)

instance HasConfig HoneycombClient where
  configL = lens clientConfig (\c conf -> c { clientConfig = conf })

type MonadHoneycomb env m = (MonadIO m, HasHoneycombClient env, MonadReader env m)

data Event = Event
  { _fields :: S.HashMap Text Value
  , _teamWriteKey :: Maybe Text
  , _dataset :: Maybe DatasetName
  , _apiHost :: Maybe Text
  , _sampleRate :: Maybe Word64
  , _timestamp :: Maybe Time
  }


post :: (MonadIO m, MonadHoneycomb env m, ToJSON a) => (Request -> m (Response b)) -> [Text] -> RequestHeaders -> a -> m (Response b)
post f pathPieces hs x = do
  Config{..} <- view (honeycombClientL . configL)
  let req = defaultRequest 
        { method = methodPost
        , host = "api.honeycomb.io"
        , port = 443
        , path = T.encodeUtf8 $ T.intercalate "/" pathPieces
        , secure = True
        , requestHeaders = hs ++
            [ (hUserAgent, "libhoneycomb-haskell/0.1")
            , (hContentType, "application/json")
            , ("X-Honeycomb-Team", T.encodeUtf8 teamWritekey)
            ]
        -- TODO
        , requestBody = RequestBodyLBS $ encode x
        }
  f req

decodeJSON :: FromJSON a => Response L.ByteString -> Response (Either String a)
decodeJSON = fmap eitherDecode