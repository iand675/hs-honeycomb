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
import Lens.Micro.Extras
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans
import Control.Monad.Except (ExceptT(..))

class MonadIO m => MonadTrace m where
  askTraceContext :: m TraceContext 
  default askTraceContext :: (MonadReader env m, HasTraceContext env) => m TraceContext
  askTraceContext = asks (view traceContextL)

  localTraceContext :: (TraceContext -> TraceContext) -> m a -> m a
  default localTraceContext :: (MonadReader env m, HasTraceContext env) => (TraceContext -> TraceContext) -> m a -> m a
  localTraceContext f = local (traceContextL %~ f)

  spanning :: Text -> m a -> m a
  default spanning :: (MonadUnliftIO m) => Text -> m a -> m a
  spanning name_ n = do
    TraceContext{..} <- askTraceContext
    Raw.spanning tcTracer (trace tcSpan) tcSvc (Just $ spanId tcSpan) name_ tcErrorAnnotators $ \s -> 
      localSpan (const s) n

instance (MonadUnliftIO m, HasTraceContext env) => MonadTrace (ReaderT env m)
instance MonadTrace m => MonadTrace (ExceptT e m) where
  askTraceContext = lift askTraceContext
  localTraceContext f (ExceptT m) = ExceptT $ localTraceContext f m
  -- TODO I'm not sure whether this is quite the implementation we want, but it's okay enough to go on
  -- for now
  spanning n (ExceptT m) = ExceptT $ spanning n m

askTrace :: (MonadTrace m) => m MutableTrace
askTrace = view (spanL . traceL) <$> askTraceContext

localTrace :: (MonadTrace m) => (MutableTrace -> MutableTrace) -> m a -> m a
localTrace f = localTraceContext (spanL . traceL %~ f)

addTraceField :: (MonadTrace m, ToJSON a) => Text -> a -> m ()
addTraceField n x = do
  t <- askTrace
  Raw.addTraceField t n x

addTraceFields :: (MonadTrace m) => HashMap Text Value -> m ()
addTraceFields m = do
  t <- askTrace
  Raw.addTraceFields t m

askSpan :: (MonadTrace m) => m MutableSpan
askSpan = view spanL <$> askTraceContext

localSpan :: (MonadTrace m) => (MutableSpan -> MutableSpan) -> m a -> m a
localSpan f = localTraceContext (spanL %~ f)

askServiceName :: (MonadTrace m) => m ServiceName
askServiceName = view serviceNameL <$> askTraceContext

localServiceName :: (MonadTrace m) => (ServiceName -> ServiceName) -> m a -> m a
localServiceName f = localTraceContext (serviceNameL %~ f)

askErrorAnnotators :: (MonadTrace m) => m [SpanErrorAnnotator]
askErrorAnnotators = view spanErrorHandlerL <$> askErrorAnnotators

localErrorAnnotators :: (MonadTrace m) => ([SpanErrorAnnotator] -> [SpanErrorAnnotator]) -> m a -> m a
localErrorAnnotators f = localTraceContext (spanErrorHandlerL %~ f)

asService :: (MonadTrace m) => ServiceName -> m a -> m a
asService svc = localTraceContext (serviceNameL .~ svc)

addLink :: (MonadTrace m) => Text -> Raw.Link -> HashMap Text Value -> m ()
addLink name_ l fs = do
  TraceContext{..} <- askTraceContext
  Raw.addLink tcTracer (trace tcSpan) tcSvc (spanId tcSpan) name_ l fs

addEvent :: (MonadTrace m) => Text -> HashMap Text Value -> m ()
addEvent n fs = do
  TraceContext{..} <- askTraceContext
  Raw.addEvent tcTracer (trace tcSpan) tcSvc (spanId tcSpan) n fs

addSpanField :: (MonadTrace m, ToJSON a) => Text -> a -> m ()
addSpanField t x = do
  s <- askSpan
  Raw.addSpanField s t x

addSpanFields :: (MonadTrace m) => HashMap Text Value -> m ()
addSpanFields m = do
  s <- askSpan
  Raw.addSpanFields s m