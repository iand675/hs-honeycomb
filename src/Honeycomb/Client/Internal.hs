{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Honeycomb.Client.Internal where

import Chronos
import Control.Concurrent.Async
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, encode)
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import Data.Word (Word64)
import Honeycomb.Config
import qualified Honeycomb.Config as Config
import Honeycomb.Types
import Lens.Micro
import Lens.Micro.Extras
import Network.HTTP.Client
import Network.HTTP.Types
import System.Random.MWC
import UnliftIO.STM (TBQueue)

data HoneycombClient = HoneycombClient
  { clientConfig :: Config
  , clientGen :: GenIO
  , clientEventBuffer :: TBQueue (IO ())
  -- ^ Subject to change
  -- TODO this respects dispatching to custom host/dataset/writekey/etc, but needs a means of
  -- using the bulk events API instead of dispatching a bunch of single event calls.
  , -- , clientQueueMap :: Map ThreadId
    clientWorkers :: [Async ()]
  }

class HasConfig a => HasHoneycombClient a where
  honeycombClientL :: Lens' a HoneycombClient

instance HasHoneycombClient HoneycombClient where
  honeycombClientL = lens id (\_ new -> new)

instance HasConfig HoneycombClient where
  configL = lens clientConfig (\c conf -> c {clientConfig = conf})

type MonadHoneycomb env m = (MonadIO m, HasHoneycombClient env, MonadReader env m)

-- | Weaker version of 'MonadHoneycomb' which only provides a config. Useful for
--   just doing HTTP requests without using the events sender.
type MonadHoneycombConfig env m = (HasConfig env, MonadReader env m)

data Event = Event
  { fields :: S.HashMap Text Value
  , teamWriteKey :: Maybe Text
  , dataset :: Maybe DatasetName
  , apiHost :: Maybe Text
  , sampleRate :: Maybe Word64
  , timestamp :: Maybe Time
  }

defaultHoneycombRequest :: Text -> [Text] -> [Header] -> Text -> Request
defaultHoneycombRequest apiHost pathPieces hs key =
  defaultRequest
    { host = T.encodeUtf8 apiHost
    , port = 443
    , path = T.encodeUtf8 $ T.intercalate "/" pathPieces
    , secure = True
    , requestHeaders =
        hs
          ++ [ (hUserAgent, "libhoneycomb-haskell/0.0.0.1")
             , (hContentType, "application/json")
             , ("X-Honeycomb-Team", T.encodeUtf8 key)
             ]
    }

post :: (MonadIO m, MonadHoneycombConfig env m, ToJSON a) => (Request -> m (Response b)) -> [Text] -> RequestHeaders -> a -> m (Response b)
post f pathPieces hs x = do
  Config {..} <- asks (view configL)
  let req =
        (defaultHoneycombRequest apiHost pathPieces hs teamWritekey)
          { method = methodPost
          , -- TODO
            requestBody = RequestBodyLBS $ encode x
          }
  f req

get :: (MonadIO m, MonadHoneycombConfig env m, HasConfig env) => (Request -> m (Response b)) -> [Text] -> RequestHeaders -> m (Response b)
get f pathPieces hs = do
  Config {..} <- asks (view configL)
  let req =
        (defaultHoneycombRequest apiHost pathPieces hs teamWritekey)
          { method = methodGet
          }
  f req

put :: (MonadIO m, MonadHoneycomb env m, FromJSON a) => (Request -> m (Response b)) -> [Text] -> [(Text, Text)] -> m a
put = undefined

delete :: (MonadIO m, MonadHoneycomb env m, FromJSON a) => (Request -> m (Response b)) -> [Text] -> [(Text, Text)] -> m a
delete = undefined

decodeJSON :: FromJSON a => Response L.ByteString -> Response (Either String a)
decodeJSON = fmap eitherDecode
