{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Tracing.Monad where
import Control.Monad.IO.Class
import Control.Monad.Morph
import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Honeycomb.Tracing
import qualified Honeycomb.Tracing.Raw as Raw
import UnliftIO
import Control.Monad.Reader.Class
import Lens.Micro
import Lens.Micro.Mtl
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans (lift)
import Control.Monad.Trans

type MonadSpan env m =
  ( MonadIO m
  , MonadReader env m
  , HasSpan env
  )

askTrace :: (MonadReader env m, HasTrace env) => m MutableTrace
askTrace = view traceL

localTrace :: (MonadReader env m, HasTrace env) => (MutableTrace -> MutableTrace) -> m a -> m a
localTrace f = local (traceL %~ f)

askSpan :: (MonadReader env m, HasSpan env) => m MutableSpan
askSpan = view spanL

localSpan :: (MonadReader env m, HasSpan env) => (MutableSpan -> MutableSpan) -> m a -> m a
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
  ( MonadIO m
  , MonadReader env m
  , HasSpan env
  , HasServiceName env
  , HasTracer env
  , HasSpanErrorHandler env
  )

asService :: (MonadReader env m, HasServiceName env) => ServiceName -> m a -> m a
asService svc = local (serviceNameL .~ svc)

spanning :: (MonadTrace env m, MonadUnliftIO m, HasSpanErrorHandler env) => Text {- ^ name -} -> m a -> m a
spanning name_ = spanning' name_ id

-- | This can be used when you're in a monad transformer stack where the current Monad doesn't admit an instance
-- of 'MonadUnliftIO'. The natural transformation function can be used like so:
--
-- >>> spanning' "foo" (\action -> ask >>= \env -> liftIO $ runReaderT action env) m
--
spanning' :: (MonadTrace env m, MonadUnliftIO n, MonadTrace env n, HasSpanErrorHandler env) => Text {- ^ name -} -> (forall b. n b -> m b) -> n a -> m a
spanning' name_ f n = do
  svc <- askServiceName
  span_ <- askSpan
  errorHandler <- askErrorHandler
  case errorHandler of
    SpanErrorHandler errF -> do
      f $ Raw.spanning (trace span_) svc (Just $ spanId span_) name_ (\s e -> liftIO $ errF s e) $ \s -> do
        localSpan (const s) n

-- | Useful for doing things that only require @ReaderT env IO@ in order to avoid the 'MonadUnliftIO' constraint
-- on simple functions that don't need to make callbacks and only call out to IO.
spanningIO :: 
   ( MonadTrace env m
   )
  => Text 
  -> ReaderT env IO a 
  -> m a
spanningIO name_ = spanning' name_ (\action -> ask >>= \env -> liftIO $ runReaderT action env)

-- | Useful for things like persistent's usage of runDB placing the trace monad a level down in the reader stack.
spanningLifted :: (MonadTrace env m, MonadTrans t, MFunctor t, MonadUnliftIO m, MonadUnliftIO (t m), HasSpanErrorHandler env) => Text -> t m a -> t m a
spanningLifted name_ n = do
  svc <- lift askServiceName
  span_ <- lift askSpan
  errorHandler <- lift askErrorHandler
  case errorHandler of
    SpanErrorHandler errF -> do
      Raw.spanningLifted (trace span_) svc (Just $ spanId span_) name_ (\s e -> liftIO $ errF s e) $ \s -> do
        hoist (localSpan (const s)) n

addLink :: (MonadTrace env m) => Text -> Raw.Link -> HashMap Text Value -> m ()
addLink name_ l fs = do
  svc <- askServiceName
  span_ <- askSpan
  Raw.addLink (trace span_) svc (spanId span_) name_ l fs


addEvent :: (MonadTrace env m) => Text -> HashMap Text Value -> m ()
addEvent n fs = do
  svc <- askServiceName
  span_ <- askSpan
  Raw.addEvent (trace span_) svc (spanId span_) n fs

addSpanField :: (MonadIO m, MonadReader env m, HasSpan env, ToJSON a) => Text -> a -> m ()
addSpanField t x = do
  s <- askSpan
  Raw.addSpanField s t x

addSpanFields :: (MonadIO m, MonadReader env m, HasSpan env) => HashMap Text Value -> m ()
addSpanFields m = do
  s <- askSpan
  Raw.addSpanFields s m