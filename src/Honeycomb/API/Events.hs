{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.API.Events where
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
import Network.HTTP.Client
import Network.HTTP.Types


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

sendEvent :: MonadIO m => HoneycombClient -> DatasetName -> Event -> m ()
sendEvent client ds e = liftIO $ do
  r <- post httpLbs client ["1", "events", fromDatasetName ds] hs $ eventData e
  case (statusCode $ responseStatus r, responseBody r) of
    (400, "unknown API key - check your credentials") -> throw UnknownApiKey
    (400, "request body is too large") -> throw RequestBodyTooLarge
    (400, "request body is malformed and cannot be read as JSON") -> throw MalformedRequestBody
    (403, "event dropped due to administrative throttling") -> throw EventDroppedDueToThrottling
    (429, "event dropped due to administrative blacklist") -> throw EventDroppedDueToBlacklist
    (429, "request dropped due to rate limiting") -> throw RequestDroppedDueToRateLimiting
    (200, _) -> pure ()
    (_, str) -> throw $ UnrecognizedError (responseStatus r) str

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

Size limitations may be addressed by gzip ping request bodies. Be sure to set the Content-Encoding: gzip
-}
newtype BatchOptions = UseGZip Bool

sendBatchedEvents :: MonadIO m => HoneycombClient -> DatasetName -> Vector Event -> m (Vector BatchResponse)
sendBatchedEvents c = sendBatchedEvents_ c (UseGZip False)

newtype BatchResponse = BatchResponse { batchResponseStatus :: Int }
  deriving (Show)

instance FromJSON BatchResponse where
  parseJSON = withObject "BatchResponse" $ \o -> BatchResponse <$> (o .: "status")

sendBatchedEvents_ :: MonadIO m => HoneycombClient -> BatchOptions -> DatasetName -> Vector Event -> m (Vector BatchResponse)
sendBatchedEvents_ c _ ds events = liftIO $ do
  r <- post httpLbs c ["1", "batch", fromDatasetName ds] [] events
  case responseBody $ decodeJSON r of
    Left err -> throw $ MalformedJSONResponse err (responseBody r)
    Right ok -> pure ok
