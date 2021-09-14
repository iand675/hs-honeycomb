{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Honeycomb.Tracing.Instrumentation.HttpClientSimple 
  ( httpBS
  , httpLBS
  , httpNoBody
  , httpJSON
  , httpJSONEither
  , httpSource
  , withResponse
  , httpLbs
  , module X
  ) where

import Network.HTTP.Simple as X hiding 
  ( httpBS
  , httpLBS
  , httpNoBody
  , httpJSON
  , httpJSONEither
  , httpSink
  , httpSource
  , withResponse
  , httpLbs
  )
import qualified Network.HTTP.Simple as Simple
import Honeycomb.Tracing.Monad
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Aeson
import Conduit
import Honeycomb.Tracing.Instrumentation.HttpClient (addRequestFields, addResponseFields, propagateTraceInfo)
import Honeycomb.Tracing.Fields
import Data.Text (Text)
import Control.Monad.Reader (MonadReader)
import Honeycomb.Tracing.Propagation (HasCodecs)

annotateBasics :: MonadTrace m => Request -> (Request -> m (Response a)) -> m (Response a)
annotateBasics req f = do
  addSpanField packageField ("http-conduit" :: Text)
  addSpanField typeField ("http_client" :: Text)
  addRequestFields req
  resp <- f req
  addResponseFields resp
  pure resp

httpBS :: (MonadTrace m, MonadReader env m, HasCodecs env [Header]) => Request -> m (Response Strict.ByteString)
httpBS req = spanning "httpBS" $ do
  req' <- propagateTraceInfo req
  annotateBasics req' Simple.httpBS

httpLBS :: (MonadTrace m, MonadReader env m, HasCodecs env [Header]) => Request -> m (Response Lazy.ByteString)
httpLBS req = spanning "httpLBS" $ do
  req' <- propagateTraceInfo req
  annotateBasics req' Simple.httpLBS

httpNoBody :: (MonadTrace m, MonadReader env m, HasCodecs env [Header]) => Request -> m (Response ())
httpNoBody req = spanning "httpNoBody" $ do
  req' <- propagateTraceInfo req
  annotateBasics req' Simple.httpNoBody

httpJSON :: (MonadTrace m, MonadReader env m, HasCodecs env [Header], FromJSON a) => Request -> m (Response a)
httpJSON req = spanning "httpJSON" $ do
  req' <- propagateTraceInfo req
  annotateBasics req' Simple.httpJSON

httpJSONEither :: (MonadTrace m, MonadReader env m, HasCodecs env [Header], FromJSON a) => Request -> m (Response (Either JSONException a))
httpJSONEither req = spanning "httpJSONEither" $ do
  req' <- propagateTraceInfo req
  annotateBasics req' Simple.httpJSONEither

-- TODO
httpSink :: (MonadTrace m, MonadReader env m, HasCodecs env [Header], MonadUnliftIO m) => Request -> (Response () -> ConduitM Strict.ByteString Void m a) -> m a
httpSink req f = do
  req' <- propagateTraceInfo req
  Simple.httpSink req' f

-- TODO
httpSource :: (MonadTrace m, MonadReader env m, HasCodecs env [Header], MonadResource m, MonadIO n) => Request -> (Response (ConduitM i Strict.ByteString n ()) -> ConduitM i o m r) -> ConduitM i o m r
httpSource req f = do
  req' <- lift $ propagateTraceInfo req
  Simple.httpSource req' f

-- TODO
withResponse :: (MonadTrace m, MonadReader env m, HasCodecs env [Header], MonadUnliftIO m, MonadIO n) => Request -> (Response (ConduitM i Strict.ByteString n ()) -> m a) -> m a
withResponse req f = do
  req' <- propagateTraceInfo req
  Simple.withResponse req' f

httpLbs :: (MonadTrace m, MonadReader env m, HasCodecs env [Header]) => Request -> m (Response Lazy.ByteString)
httpLbs = httpLBS
