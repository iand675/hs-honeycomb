{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Tracing.Instrumentation.Wai where

import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT) )
import Data.Aeson
import qualified Data.Text.Encoding as T
import Data.Typeable ( typeOf )
import Data.Vault.Lazy ( insert, lookup, newKey, Key )
import Honeycomb.Tracing ( MutableSpan, MutableTrace, Tracer (tracerServiceName), tracerPropagationCodecs )
import Honeycomb.Tracing.Fields
    ( typeField,
      packageField,
      requestHostField,
      requestMethodField,
      requestHttpVersionField,
      requestPathField,
      requestSecureField,
      requestContentLengthField,
      requestRemoteAddressField,
      userAgentField,
      forwardForHeaderField,
      requestErrorField,
      requestErrorDetailField,
      requestContentTypeField,
      requestAcceptField,
      requestQueryParamsField,
      statusCodeField,
      responseContentTypeField, packageVersionField )
import Honeycomb.Tracing.Raw ( initializeTraceContext, sendLocalTraceSpans, spanning, addSpanField, addSpanFields )
import Network.HTTP.Types
    ( Status(statusCode), hAccept, hContentType )
import Network.Wai
    ( responseHeaders,
      responseStatus,
      Middleware,
      Request(rawPathInfo, requestHeaderHost, requestMethod, httpVersion,
              isSecure, requestBodyLength, remoteHost, requestHeaderUserAgent,
              requestHeaders, rawQueryString, vault),
      RequestBodyLength(KnownLength, ChunkedBody) )
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.HashMap.Strict as H
import UnliftIO ( SomeException, bracket )
import Data.Text (Text)
import Honeycomb.Tracing.Propagation

traceKey :: Key MutableTrace
traceKey = unsafePerformIO newKey
{-# NOINLINE traceKey #-}

spanKey :: Key MutableSpan
spanKey = unsafePerformIO newKey
{-# NOINLINE spanKey #-}

lookupTrace :: Request -> Maybe MutableTrace
lookupTrace = Data.Vault.Lazy.lookup traceKey . vault

lookupSpan :: Request -> Maybe MutableSpan
lookupSpan = Data.Vault.Lazy.lookup spanKey . vault

beelineMiddleware :: Tracer -> Middleware
beelineMiddleware conf app req responder = runReaderT (do
  -- Inherit trace if a recognized trace format is hanging about
  let propagatedInfo = getContextFromCodecs (tracerPropagationCodecs conf) (requestHeaders req)
  bracket (initializeTraceContext propagatedInfo) sendLocalTraceSpans $ \trace -> do
    -- Default to path for span name, but can be overridden
    let path = T.decodeUtf8 $ rawPathInfo req
    spanning trace (tracerServiceName conf) Nothing {- <- TODO pull from headers? -} path errorHandler $ \span_ -> do
      addSpanFields span_ $ H.fromList
        [ (typeField, String "http_server")
        , (packageField, String "wai/warp")
        -- TODO CPP macro? VERSION_wai/VERSION_warp
        {-
        , ( packageVersionField
          , object 
            [ "wai" .= ("VERSION_wai" :: Text)
            , "warp" .= ("VERSION_warp" :: Text)
            ]
          )
        -}
        , (requestHostField, toJSON $ T.decodeUtf8 <$> requestHeaderHost req)
        , (requestMethodField, toJSON $ T.decodeUtf8 $ requestMethod req)
        , (requestHttpVersionField, toJSON $ show $ httpVersion req)
        , (requestPathField, toJSON path)
        , (requestSecureField, toJSON $ isSecure req)
        , ( requestContentLengthField
          , case requestBodyLength req of
              ChunkedBody -> Null
              KnownLength len -> toJSON len
          )
        , (requestRemoteAddressField, toJSON $ show $ remoteHost req)
        , (userAgentField, toJSON $ T.decodeUtf8 <$> requestHeaderUserAgent req)
        , (forwardForHeaderField, toJSON $ fmap T.decodeUtf8 $ Prelude.lookup "X-Forwarded-For" $ requestHeaders req)
        , (requestContentTypeField, toJSON $ fmap T.decodeUtf8 $ Prelude.lookup hContentType $ requestHeaders req)
        , (requestAcceptField, toJSON $ fmap T.decodeUtf8 $ Prelude.lookup hAccept $ requestHeaders req)
        , (requestQueryParamsField, toJSON $ T.decodeUtf8 $ rawQueryString req)
        ]
      {-
        -- TODO , (requestSchemeField, )
        -- TODO , (requestAjaxField, )
        -- , (forwardProtoHeaderField, )
        ]
      -}
      let vault' = insert spanKey span_ $ insert traceKey trace $ vault req
          req' = req 
            { vault = vault' 
            {-
            , requestHeaders = (requestHeaders req)
                {
                }
            -}
            }
      liftIO $ app req' $ \resp -> do
        addSpanFields span_ $ H.fromList 
          [ (statusCodeField, toJSON $ statusCode $ responseStatus resp)
          , (responseContentTypeField, toJSON $ fmap T.decodeUtf8 $ Prelude.lookup hContentType $ responseHeaders resp)
          ]
        responder resp) conf
  where
    errorHandler :: MutableSpan -> SomeException -> ReaderT Tracer IO ()
    errorHandler span_ err = do
      addSpanField span_ requestErrorField $ show $ typeOf err
      addSpanField span_ requestErrorDetailField $ show err
      return ()
