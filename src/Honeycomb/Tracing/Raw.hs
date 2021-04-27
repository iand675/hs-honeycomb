{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Tracing.Raw 
  ( initializeTraceContext
  , sendLocalTraceSpans
  -- , setSampleRate
  , addTraceField
  , addTraceFields
  , newSpan
  , closeSpan
  , addSpanField
  , addSpanFields
  , addEvent
  , addLink
  , Link(..)
  , spanning
  , spanningLifted
  ) where
import Chronos
import UnliftIO
import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import Honeycomb.Client.Internal
import Honeycomb.Tracing
import Honeycomb.Tracing.Ids.TraceIdProvider
import Lens.Micro hiding (view)
import Lens.Micro.Mtl
import Honeycomb.Client
import Honeycomb.Tracing.Fields
import Data.Word
import Control.Monad.Trans (lift)
import Conduit (MonadTrans)
import Honeycomb.Tracing.Propagation

-- | Create the basic context necessary to create spans for a given trace
initializeTraceContext :: (MonadIO m, MonadReader r m, HasTracer r) 
  => PropagationContext -- ^ optionally initialize a trace with context from the outside world
  -> m MutableTrace
initializeTraceContext externalCtx = do
  Tracer{..} <- view tracerL
  tId <- case externalCtx of
    EmptyContext -> liftIO $ generateTraceId tracerIdGenerator
    Context{..} -> pure propagatedTraceId
  let fs = case externalCtx of
        EmptyContext -> mempty
        Context{..} -> propagatedTraceFields
      ds = case externalCtx of
        EmptyContext -> Nothing
        Context{..} -> propagatedDataset
  liftIO $
    Trace tId tracerServiceName
      <$> liftIO (newIORef Nothing)
      <*> liftIO (newIORef mempty)
      <*> liftIO (newIORef fs)
      <*> pure ds

-- TODO not sure if this should exist or not
setSampleRate :: (MonadIO m, HasTrace t) => t -> Word64 -> m ()
setSampleRate t x = writeIORef (t ^. traceL . to traceSample) (Just x)

makeEvent :: HashMap Text Value -> (Maybe Word64, HashMap Text Value) -> ImmutableSpan -> Maybe Event
makeEvent _ _ EmptySpan = Nothing
makeEvent traceFields (msampleRate, postprocessedSpanFields) Span{..} = do
  let spanFields = postprocessedSpanFields
      startT = startTime
      endT = fromMaybe startT endTime
      addParentId = maybe id (\x xs -> (parentIdField, toJSON x) : xs) parentSpan
      officialFields =
        addParentId
          [ (spanNameField, String name),
            (serviceNameField, toJSON service),
            (durationField, toJSON (((fromIntegral $ getTimespan $ width $ TimeInterval startT endT) / 1_000_000) :: Double)),
            (spanIdField, toJSON spanId),
            (traceIdField, toJSON $ traceId trace)
            -- (metaSpanTypeField, String metaTypeSpanEventValue)
          ]
  Just $ event
    { _fields = traceFields <> spanFields <> H.fromList officialFields
    , _timestamp = Just startT
    , _sampleRate = msampleRate
    , _dataset = traceDataset trace
    }

-- | This technically only closes the trace in the local sense. Additional spans may
-- be sent to Honeycomb (e.g. from propagation) as long as they fall within the root
-- span's start/end times.
sendLocalTraceSpans :: (MonadIO m, MonadReader env m, HasTracer env) => MutableTrace -> m ()
sendLocalTraceSpans mutT = do
  tracer <- view tracerL
  t <- freeze mutT
  let fs = traceFields t
      r = H.elems $ traceSpans t
      allSpansAreClosed = all (isJust . endTime) r
  unless allSpansAreClosed $ do
    -- TODO better warning or something.
    liftIO $ putStrLn "Warning: some spans for trace are not closed prior to sending."
  let postprocessedEvents = map (\s -> makeEvent fs (_postprocessEvent tracer s) s) r
  mapM_ (send tracer) $ catMaybes postprocessedEvents
  where
    _postprocessEvent c s = case tracerSpanPostProcessor c of
      Nothing -> (Nothing, fields s)
      Just f -> f s

newSpan ::
  (MonadIO m) =>
  Tracer ->
  MutableTrace ->
  ServiceName -> -- Service
  Maybe SpanId -> -- Parent ID
  Text -> -- Name
  m MutableSpan
newSpan Tracer{..} t svc pid n = liftIO $ do
  _id <- generateSpanId tracerIdGenerator
  ts <- now
  newSpan_ <-
    Span _id n svc
      <$> newIORef ts
      <*> newIORef Nothing
      <*> pure pid
      <*> pure t
      <*> newIORef H.empty
  modifyIORef' (traceSpans t) (H.insert _id newSpan_)
  pure newSpan_

closeSpan :: MonadIO m => MutableSpan -> m ()
closeSpan Span {..} = liftIO $ do
  closeTime <- now
  modifyIORef' endTime (\t -> t <|> pure closeTime)
closeSpan EmptySpan = pure ()

spanAnnotatorToHandler :: MonadIO m => MutableSpan -> SpanErrorAnnotator -> Handler m a
spanAnnotatorToHandler s (SpanErrorAnnotator f) = Handler $ \e -> liftIO (f s e) *> throwIO e

spanning ::
  (MonadUnliftIO m) =>
  Tracer ->
  MutableTrace ->
  ServiceName ->
  Maybe SpanId ->
  Text ->
  -- | Annotate spans with specific exceptions
  [SpanErrorAnnotator] ->
  (MutableSpan -> m a) ->
  m a
spanning tr t svc pid n errorAnnotators f = bracket (newSpan tr t svc pid n) closeSpan $ \s -> do
  f s `catches` map (spanAnnotatorToHandler s) errorAnnotators

spanningLifted ::
  (MonadTrans t, MonadUnliftIO m, MonadUnliftIO (t m)) =>
  Tracer ->
  MutableTrace ->
  ServiceName ->
  Maybe SpanId ->
  Text ->
  -- | Annotate spans with specific exceptions
  [SpanErrorAnnotator] ->
  (MutableSpan -> t m a) ->
  t m a
spanningLifted tr t svc pid n errorAnnotators f = bracket (lift $ newSpan tr t svc pid n) closeSpan $ \s -> do
  f s `catches` map (spanAnnotatorToHandler s) errorAnnotators

addTraceField :: (MonadIO m, ToJSON a) => MutableTrace -> Text -> a -> m ()
addTraceField trace fieldName x = modifyIORef' (traceFields trace) (H.insert fieldName $ toJSON x)

addSpanField :: (MonadIO m, ToJSON a) => MutableSpan -> Text -> a -> m ()
addSpanField EmptySpan _ _ = pure ()
addSpanField Span{..} fieldName x = do
  modifyIORef' fields (H.insert fieldName $ toJSON x)

addTraceFields :: (MonadIO m) => MutableTrace -> HashMap Text Value -> m ()
addTraceFields trace fs = modifyIORef' (traceFields trace) (<> fs)

addSpanFields :: (MonadIO m) => MutableSpan -> HashMap Text Value -> m ()
addSpanFields EmptySpan _ = pure ()
addSpanFields Span{..} fs = modifyIORef' fields (<> fs)

data Link = Link
  { linkSpanId :: SpanId
  , linkTraceId :: TraceId
  }

-- | A span may be linked to zero or more other spans or traces that are causally related. 
-- They can point to another span within the same trace or a span in a different trace. 
-- The tracing data model focuses on the parent-child relationship between spans, 
-- and most spans can be adequately described with just a span id, a parent span id, and a trace ID. 
-- However, in some special cases, it may be useful to describe a less direct causal relationship between spans. 
-- Links are optional, but can be useful for expressing a causal relationship to one or more spans or traces elsewhere 
-- in your dataset. Links can be used to represent batched operations where a span was initiated by multiple 
-- initiating spans, each representing a single incoming item being processed in the batch.
addLink :: (MonadIO m) => Tracer -> MutableTrace -> ServiceName -> SpanId -> Text -> Link -> HashMap Text Value -> m ()
addLink Tracer{..} t svc pid n Link{..} fs = liftIO $ do
  _id <- generateSpanId tracerIdGenerator
  ts <- now
  newSpan_ <-
    Span _id n svc
      <$> newIORef ts
      <*> newIORef (Just ts)
      <*> pure (Just pid)
      <*> pure t
      <*> newIORef (fs <> H.fromList
          [ (metaAnnotationTypeField, String "link")
          , ("trace.link.span_id", toJSON linkSpanId)
          , ("trace.link.trace_id", toJSON linkTraceId)
          ]
        )
  modifyIORef' (traceSpans t) (H.insert _id newSpan_)

-- | Span Events are timestamped structured logs (aka events), without a duration. 
-- They occur during the course of a Span and can be thought of as annotations on the Span. 
-- For example, you might have a Span that represents a specific operation in your service. 
-- That operation could have a loop, in which a non-fatal error can occur. 
-- If you write the error to an error field on the Span, you’ll overwrite any previous errors 
-- (from previous loop iterations) recorded in that field. This is a perfect use case for Span Events, 
-- the error events can be attached as Span Event Annotations and you capture all the errors.
addEvent :: (MonadIO m) => Tracer -> MutableTrace -> ServiceName -> SpanId -> Text -> HashMap Text Value -> m ()
addEvent Tracer{..} t svc pid n fs = liftIO $ do
  _id <- generateSpanId tracerIdGenerator
  ts <- now
  -- putStrLn ("Starting span: " ++ show _id ++ " at " ++ show ts)
  newSpan_ <-
    Span _id n svc
      <$> newIORef ts
      <*> newIORef (Just ts)
      <*> pure (Just pid)
      <*> pure t
      <*> newIORef (fs <> H.fromList
          [ (metaAnnotationTypeField, String "span_event")
          ]
        )
  modifyIORef' (traceSpans t) (H.insert _id newSpan_)