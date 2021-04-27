{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Honeycomb.Tracing.Instrumentation.Yesod where

import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Typeable ()
import Honeycomb.Tracing
    ( HasServiceName(..),
      HasSpan(..),
      HasSpanErrorAnnotators(..),
      HasTracer(..), SpanErrorAnnotator(..), HasTrace(..), recordAnchoredErrorHandler, HasTraceContext(..) )-- ( Span, addField, HasTraceConfig (getTraceConfig), HasSpan )
import Honeycomb.Tracing.Fields ( spanNameField )
import Honeycomb.Tracing.Instrumentation.Wai (lookupSpan)
import Honeycomb.Types ()
import Network.Wai (requestMethod)
import Yesod.Core
    ( getCurrentRoute, waiRequest, HandlerFor, RenderRoute(Route) )
import Yesod.Core.Types
    ( HandlerData(handlerEnv),
      RunHandlerEnv(rheSite),
      HandlerContents(..),
      ErrorResponse(..) )
import Honeycomb.Tracing.Monad
import Lens.Micro (lens, Lens', (.~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (local)
import qualified Honeycomb.Tracing.Raw as Raw
import Control.Exception

handlerEnvL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
handlerEnvL = lens handlerEnv (\h e -> h { handlerEnv = e })

rheSiteL :: Lens' (RunHandlerEnv child site) site
rheSiteL = lens rheSite (\rhe new -> rhe { rheSite = new })

instance HasTraceContext site => HasTraceContext (RunHandlerEnv child site) where
  traceContextL = rheSiteL . traceContextL

instance HasTraceContext site => HasTraceContext (HandlerData child site) where
  traceContextL = handlerEnvL . traceContextL

instance HasTraceContext site => MonadTrace (HandlerFor site)

beelineMiddleware :: (Show (Route site), MonadTrace (HandlerFor site)) => HandlerFor site res -> HandlerFor site res
beelineMiddleware handler = do
  req <- waiRequest
  case lookupSpan req of
    Nothing -> handler -- TODO set up a root span & trace if not instrumented in WAI middleware
    Just span_ -> localSpan (const span_) $ localErrorAnnotators (recordAnchoredErrorHandler span_ :) $ do
      route <- getCurrentRoute
      case route of
        Nothing -> handler
        Just r -> do
          -- TODO Abusing the show instance like this is not ideal
          addSpanField "request.route" $ head $ words $ show r
          addSpanField spanNameField (C.unpack (C.map toLower (requestMethod req)) ++ head (words $ show r))
          handler

