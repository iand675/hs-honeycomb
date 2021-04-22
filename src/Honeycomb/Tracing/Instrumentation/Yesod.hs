{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Honeycomb.Tracing.Instrumentation.Yesod where

import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Typeable ()
import Honeycomb.Tracing hiding (addField, addFields)-- ( Span, addField, HasTraceConfig (getTraceConfig), HasSpan )
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
import Lens.Micro (lens, Lens')

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

instance HasTraceConfig site => HasTraceConfig (RunHandlerEnv child site) where
  traceConfigL = rheSiteL . traceConfigL

instance HasTraceConfig site => HasTraceConfig (HandlerData child site) where
  traceConfigL = handlerEnvL . traceConfigL

instance HasTraceConfig site => HasServiceName (RunHandlerEnv child site) where
  serviceNameL = rheSiteL . traceConfigL . serviceNameL

instance HasTraceConfig site => HasServiceName (HandlerData child site) where
  serviceNameL = handlerEnvL . serviceNameL

beelineMiddleware :: (Show (Route site), HasSpan site) => HandlerFor site res -> HandlerFor site res
beelineMiddleware handler = do
  route <- getCurrentRoute
  req <- waiRequest
  case route of
    Nothing -> handler
    Just r -> case lookupSpan req of
      Nothing -> handler
      Just span_ -> do
        -- TODO Abusing the show instance like this is not ideal
        addField "request.route" $ head $ words $ show r
        addField spanNameField (C.unpack (C.map toLower (requestMethod req)) ++ (head $ words $ show r))
        localSpan (const span_) handler