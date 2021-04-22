{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Honeycomb.Tracing.Monad where
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Honeycomb.Tracing
import qualified Honeycomb.Tracing as Simple
import UnliftIO
import Control.Monad.Reader.Class
import Lens.Micro
import Lens.Micro.Mtl

type MonadSpan env m = 
  ( MonadIO m
  , MonadReader env m
  , HasSpan env
  )

askTrace :: (MonadReader env m, HasTrace env) => m Trace
askTrace = view traceL

localTrace :: (MonadReader env m, HasTrace env) => (Trace -> Trace) -> m a -> m a
localTrace f = local (traceL %~ f)

askSpan :: (MonadReader env m, HasSpan env) => m Span
askSpan = view spanL

localSpan :: (MonadReader env m, HasSpan env) => (Span -> Span) -> m a -> m a
localSpan f = local (spanL %~ f)

askServiceName :: (MonadReader env m, HasServiceName env) => m ServiceName
askServiceName = view serviceNameL

localServiceName :: (MonadReader env m, HasServiceName env) => (ServiceName -> ServiceName) -> m a -> m a
localServiceName f = local (serviceNameL %~ f)

askErrorHandler :: (MonadReader env m, HasSpanErrorHandler env) => m SpanErrorHandler 
askErrorHandler = view spanErrorHandlerL

localErrorHandler :: (MonadReader env m, HasSpanErrorHandler env) => (SpanErrorHandler -> SpanErrorHandler) -> m a -> m a
localErrorHandler f = local (spanErrorHandlerL %~ f)

type MonadTrace env m =
  ( MonadUnliftIO m
  , MonadReader env m
  , HasSpan env
  , HasServiceName env
  , HasTraceConfig env
  , HasSpanErrorHandler env
  )

spanning :: MonadTrace env m => Text {- ^ name -} -> m a -> m a
spanning name_ m = do
  svc <- askServiceName
  span_ <- askSpan
  errorHandler <- askErrorHandler
  case errorHandler of
    SpanErrorHandler errF -> do
      Simple.spanning (trace span_) svc (Just $ spanId span_) name_ (\s e -> liftIO $ errF s e) $ \s -> do
        localSpan (const s) m

addField :: (MonadIO m, MonadReader env m, HasSpan env, ToJSON a) => Text -> a -> m ()
addField t x = do
  s <- askSpan
  Simple.addField s t x

addFields :: (MonadIO m, MonadReader env m, HasSpan env) => HashMap Text Value -> m () 
addFields m = do
  s <- askSpan
  Simple.addFields s m