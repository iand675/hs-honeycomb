{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Tracing.Instrumentation.HttpClient 
  ( withResponse
  , httpLbs
  , httpNoBody
  , responseOpen
  , responseClose
  , module X
  -- $$ Internals for use in more exotic instrumentation cases
  , addRequestFields
  , addResponseFields
  , clientRequestErrorHandler
  ) where
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as H
import Network.HTTP.Client as X hiding (withResponse, httpLbs, httpNoBody, responseOpen, responseClose)
import qualified Network.HTTP.Client as HTTP
import Honeycomb.Tracing.Monad
import Honeycomb.Tracing.Fields
import Network.HTTP.Types.Header (hContentType)
import Data.Aeson (toJSON, Value (String))
import Data.Text.Encoding (decodeUtf8)
import UnliftIO (SomeException, MonadUnliftIO (withRunInIO))
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types (statusCode)
import Data.Typeable (typeOf)


addRequestFields :: MonadSpan env m => Request -> m ()
addRequestFields req = addSpanFields $ H.fromList
  [ (clientRequestPathField, toJSON $ decodeUtf8 $ HTTP.path req)
  , (clientRequestMethodField, toJSON $ decodeUtf8 $ HTTP.method req)
  , (clientRequestContentTypeField, toJSON $ fmap decodeUtf8 $ lookup hContentType $ HTTP.requestHeaders req)
  , (packageField, String "http-client")
  , (typeField, String "http_client")
  , (requestHostField, toJSON $ decodeUtf8 $ host req)
  -- TODO
  -- , (clientRequestContentLengthField, _)
  ]

addResponseFields :: MonadSpan env m => Response a -> m ()
addResponseFields resp = addSpanFields $ H.fromList
  [ (clientResponseContentTypeField, toJSON $ fmap decodeUtf8 $ lookup hContentType $ responseHeaders resp)
  , (clientResponseStatusCodeField, toJSON $ statusCode $ HTTP.responseStatus resp)
  -- TODO
  -- , (clientResponseContentLength, _)
  ]

-- TODO
clientRequestErrorHandler :: MonadTrace env m => SomeException -> m ()
clientRequestErrorHandler e = do
  addSpanField clientRequestErrorField $ show $ typeOf e
  addSpanField clientRequestErrorDetailField $ show e

withResponse :: MonadTrace env m => Request -> Manager -> (Response BodyReader -> m a) -> m a
withResponse req m f = spanning "withResponse" $ do
  addRequestFields req
  withRunInIO $ \runInIO -> 
    HTTP.withResponse req m $ \resp -> runInIO $ do
      addResponseFields resp
      f resp

httpLbs :: MonadTrace env m => Request -> Manager -> m (Response L.ByteString) 
httpLbs req m = spanning "httpLbs" $ do
  addRequestFields req
  resp <- liftIO $ HTTP.httpLbs req m
  addResponseFields resp
  pure resp

httpNoBody :: (MonadTrace env m) => Request -> Manager -> m (Response ()) 
httpNoBody req m = spanning "httpNoBody" $ do
  addRequestFields req
  resp <- liftIO $ HTTP.httpNoBody req m
  addResponseFields resp
  pure resp

responseOpen :: (MonadTrace env m) => Request -> Manager -> m (Response BodyReader)
responseOpen req m = spanning "responseOpen" $ do
  addRequestFields req
  resp <- liftIO $ HTTP.responseOpen req m
  addResponseFields resp
  pure resp

responseClose :: (MonadTrace env m) => Response a -> m ()
responseClose resp = spanning "responseClose" $ liftIO $ HTTP.responseClose resp
