{-# LANGUAGE OverloadedStrings #-}
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
import Honeycomb.Tracing.Instrumentation.HttpClient (addRequestFields, addResponseFields)

annotateBasics :: MonadTrace env m => Request -> (Request -> m (Response a)) -> m (Response a)
annotateBasics req f = do
  addRequestFields req
  resp <- f req
  addResponseFields resp
  pure resp

httpBS :: MonadTrace env m => Request -> m (Response Strict.ByteString)
httpBS req = spanning "httpBS" $ do
  annotateBasics req Simple.httpBS

httpLBS :: MonadTrace env m => Request -> m (Response Lazy.ByteString)
httpLBS req = spanning "httpLBS" $ do
  annotateBasics req Simple.httpLBS

httpNoBody :: MonadTrace env m => Request -> m (Response ())
httpNoBody req = spanning "httpNoBody" $ do
  annotateBasics req Simple.httpNoBody

httpJSON :: (MonadTrace env m, FromJSON a) => Request -> m (Response a)
httpJSON req = spanning "httpJSON" $ do
  annotateBasics req Simple.httpJSON

httpJSONEither :: (MonadTrace env m, FromJSON a) => Request -> m (Response (Either JSONException a))
httpJSONEither req = spanning "httpJSONEither" $ do
  annotateBasics req Simple.httpJSONEither

-- TODO
httpSink :: (MonadTrace env m, MonadUnliftIO m) => Request -> (Response () -> ConduitM Strict.ByteString Void m a) -> m a
httpSink req = Simple.httpSink req

-- TODO
httpSource :: (MonadTrace env m, MonadResource m, MonadIO n) => Request -> (Response (ConduitM i Strict.ByteString n ()) -> ConduitM i o m r) -> ConduitM i o m r
httpSource req = Simple.httpSource req

-- TODO
withResponse :: (MonadTrace env m, MonadUnliftIO m, MonadIO n) => Request -> (Response (ConduitM i Strict.ByteString n ()) -> m a) -> m a
withResponse req = Simple.withResponse req

httpLbs :: MonadTrace env m => Request -> m (Response Lazy.ByteString)
httpLbs = httpLBS