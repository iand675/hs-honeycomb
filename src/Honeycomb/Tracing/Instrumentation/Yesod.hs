{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Honeycomb.Tracing.Instrumentation.Yesod where

import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Typeable ()
import Honeycomb.Tracing
    ( HasServiceName(..),
      HasSpan(..),
      HasSpanErrorHandler(..),
      HasTracer(..) )-- ( Span, addField, HasTraceConfig (getTraceConfig), HasSpan )
import Honeycomb.Tracing.Fields ( spanNameField )
import Honeycomb.Tracing.Instrumentation.Wai (lookupSpan)
import Honeycomb.Types ()
import Network.Wai (requestMethod)
import Yesod.Core
    ( getCurrentRoute, waiRequest, HandlerFor, RenderRoute(Route) )
import Yesod.Core.Types
    ( HandlerData(handlerEnv),
      RunHandlerEnv(rheSite) )
import Honeycomb.Tracing.Monad
import Lens.Micro (lens, Lens', (.~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (local)

handlerEnvL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
handlerEnvL = lens handlerEnv (\h e -> h { handlerEnv = e })

rheSiteL :: Lens' (RunHandlerEnv child site) site
rheSiteL = lens rheSite (\rhe new -> rhe { rheSite = new })

instance HasSpanErrorHandler site => HasSpanErrorHandler (RunHandlerEnv child site) where
  spanErrorHandlerL = rheSiteL . spanErrorHandlerL

instance HasSpanErrorHandler site => HasSpanErrorHandler (HandlerData child site) where
  spanErrorHandlerL = handlerEnvL . spanErrorHandlerL

instance HasSpan site => HasSpan (RunHandlerEnv child site) where
  spanL = rheSiteL . spanL

instance HasSpan site => HasSpan (HandlerData child site) where
  spanL = handlerEnvL . spanL

instance HasTracer site => HasTracer (RunHandlerEnv child site) where
  tracerL = rheSiteL . tracerL

instance HasTracer site => HasTracer (HandlerData child site) where
  tracerL = handlerEnvL . tracerL

instance HasTracer site => HasServiceName (RunHandlerEnv child site) where
  serviceNameL = rheSiteL . tracerL . serviceNameL

instance HasTracer site => HasServiceName (HandlerData child site) where
  serviceNameL = handlerEnvL . serviceNameL

beelineMiddleware :: (Show (Route site), HasSpan site) => HandlerFor site res -> HandlerFor site res
beelineMiddleware handler = do
  req <- waiRequest
  case lookupSpan req of
    Nothing -> handler -- TODO set up a root span & trace if not instrumented in WAI middleware
    Just span_ -> local (spanL .~ span_) $ do
      route <- getCurrentRoute
      case route of
        Nothing -> handler
        Just r -> do
          -- TODO Abusing the show instance like this is not ideal
          addSpanField "request.route" $ head $ words $ show r
          addSpanField spanNameField (C.unpack (C.map toLower (requestMethod req)) ++ (head $ words $ show r))
          handler