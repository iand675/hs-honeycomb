{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.API.Events
  ( Event(..)
  , sendEvent
  , sendBatchedEvents
  , sendBatchedEvents'
  , BatchResponse(..)
  , BatchOptions(..)
  ) where
import Chronos ( timeToDatetime )
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.Vector (Vector)
import Honeycomb.Client.Internal hiding (Event)
import Honeycomb.Types
import Honeycomb.API.Types
import Network.HTTP.Simple
import Network.HTTP.Types
import Control.Lens ( (^.), to, view )
import Control.Monad.Reader (MonadReader)


data MalformedJSONResponse = MalformedJSONResponse
  { malformedJSONResponseMessage :: String
  , malformedJSONResponseBody :: L.ByteString
  }
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

data FailureResponse
  = UnknownApiKey
  | RequestBodyTooLarge
  | MalformedRequestBody
  | EventDroppedDueToThrottling
  | EventDroppedDueToBlacklist
  | RequestDroppedDueToRateLimiting
  | UnrecognizedError Status L.ByteString 
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

sendEvent :: (MonadHoneycomb client m) => Event -> m ()
sendEvent e = do
  client <- view honeycombClientL
  let ds = client ^. configL . to defaultDataset
  r <- post httpLBS ["1", "events", fromDatasetName ds] hs $ eventData e
  case (statusCode $ getResponseStatus r, getResponseBody r) of
    (400, "unknown API key - check your credentials") -> throw UnknownApiKey
    (400, "request body is too large") -> throw RequestBodyTooLarge
    (400, "request body is malformed and cannot be read as JSON") -> throw MalformedRequestBody
    (403, "event dropped due to administrative throttling") -> throw EventDroppedDueToThrottling
    (429, "event dropped due to administrative blacklist") -> throw EventDroppedDueToBlacklist
    (429, "request dropped due to rate limiting") -> throw RequestDroppedDueToRateLimiting
    (200, _) -> pure ()
    (_, str) -> throw $ UnrecognizedError (getResponseStatus r) str
  where
    hs = catMaybes
      [ (\d -> ("X-Honeycomb-Event-Time", T.encodeUtf8 $ encodeRFC3339 $ timeToDatetime d)) <$> eventTimestamp e
      , (\r -> ("X-Honeycomb-Samplerate", C.pack $ show r)) <$> eventSampleRate e
      ]

{-
There are a few limits to note in regards to the events API:

Requests to the individual event endpoint have a maximum request body size of 100KB.
Requests to the batched events endpoint have a maximum request body size of 5MB. Individual event bodies in the batch are limited to 100KB each.
The maximum number of distinct columns (fields) allowed per event is 2000.

Size limitations may be addressed by gzipping request bodies. Be sure to set the Content-Encoding: gzip
-}
newtype BatchOptions = BatchOptions 
  { useGZip :: Bool
  } deriving (Show, Read)

sendBatchedEvents :: (MonadHoneycomb client m) => Vector Event -> m (Vector BatchResponse)
sendBatchedEvents = sendBatchedEvents' (BatchOptions False)

newtype BatchResponse = BatchResponse { batchResponseStatus :: Int }
  deriving (Show)

instance FromJSON BatchResponse where
  parseJSON = withObject "BatchResponse" $ \o -> BatchResponse <$> (o .: "status")

sendBatchedEvents' :: (MonadHoneycomb client m) => BatchOptions -> Vector Event -> m (Vector BatchResponse)
sendBatchedEvents' _ events = do
  config <- view (honeycombClientL . configL)
  r <- post httpLBS ["1", "batch", fromDatasetName $ defaultDataset config] [] events
  case getResponseBody $ decodeJSON r of
    Left err -> throw $ MalformedJSONResponse err (getResponseBody r)
    Right ok -> pure ok
