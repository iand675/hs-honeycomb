{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Tracing.Raw where
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
import Lens.Micro.Mtl
import Honeycomb.Client
import Honeycomb.Tracing.Fields

newTrace :: (MonadIO m, MonadReader r m, HasTracer r) => m Trace
newTrace =
  Trace
    <$> view (tracerL . honeycombClientL)
    <*> (view tracerL >>= \Tracer{..} -> liftIO $ generateTraceId tracerIdGenerator)
    <*> liftIO (newIORef Nothing)
    <*> liftIO (newIORef mempty)
    <*> liftIO (newIORef mempty)
    <*> view tracerL

makeEvent :: HashMap Text Value -> Span -> IO (Maybe Event)
makeEvent _ EmptySpan = pure Nothing
makeEvent traceFields Span{..} = do
  spanFields <- readIORef fields
  startT <- readIORef startTime
  endT <- fromMaybe startT <$> readIORef endTime
  let addParentId = maybe id (\x xs -> (parentIdField, toJSON x) : xs) parentSpan
      officialFields =
        addParentId
          [ (spanNameField, String name),
            (serviceNameField, toJSON service),
            (durationField, toJSON (((fromIntegral $ getTimespan $ width $ TimeInterval startT endT) / 1_000_000) :: Double)),
            (spanIdField, toJSON spanId),
            (traceIdField, toJSON $ traceId trace)
            -- (metaSpanTypeField, String metaTypeSpanEventValue)
          ]
  pure $ Just $ event
    { _fields = traceFields <> spanFields <> H.fromList officialFields
    , _timestamp = Just startT
    }

closeTrace :: MonadIO m => Trace -> m ()
closeTrace t = liftIO $ do
  fs <- readIORef $ traceFields t
  r <- fmap H.elems $ readIORef $ traceSpans t
  allSpansAreClosed <- all isJust <$> mapM (readIORef . endTime) r
  unless allSpansAreClosed $ do
    -- TODO better warning or something.
    putStrLn "Warning: some spans for trace are not closed prior to sending."
  postprocessedEvents <- mapM (_postprocessEvent <=< makeEvent fs) r
  mapM_ (send (traceClient t)) $ catMaybes $ catMaybes postprocessedEvents
  where
    -- TODO
    _postprocessEvent = pure . Just

newSpan ::
  (MonadIO m, MonadReader r m, HasTracer r) =>
  Trace ->
  ServiceName -> -- Service
  Maybe SpanId -> -- Parent ID
  Text -> -- Name
  m Span
newSpan t svc pid n = do
  _id <- view tracerL >>= \Tracer{..} -> liftIO $ generateSpanId tracerIdGenerator
  liftIO $ do
    ts <- now
    -- putStrLn ("Starting span: " ++ show _id ++ " at " ++ show ts)
    newSpan_ <-
      Span _id n svc
        <$> newIORef ts
        <*> newIORef Nothing
        <*> pure pid
        <*> pure t
        <*> newIORef H.empty
    modifyIORef' (traceSpans t) (H.insert _id newSpan_)
    pure newSpan_

closeSpan :: MonadIO m => Span -> m ()
closeSpan Span {..} = liftIO $ do
  closeTime <- now
  -- putStrLn ("Closing span " ++ show spanId ++ " at " ++ show closeTime)
  modifyIORef' endTime (\t -> t <|> pure closeTime)
closeSpan EmptySpan = pure ()

spanning ::
  (MonadUnliftIO m, MonadReader r m, HasTracer r, Exception e) =>
  Trace ->
  ServiceName ->
  Maybe SpanId ->
  Text ->
  -- | Annotate spans with specific exceptions
  (Span -> e -> m ()) ->
  (Span -> m a) ->
  m a
spanning t svc pid n errorHandler f = bracket (newSpan t svc pid n) closeSpan $ \s -> do
  handle (\e -> errorHandler s e *> throwIO e) $ f s

addTraceField :: (MonadIO m, ToJSON a) => Trace -> Text -> a -> m ()
addTraceField trace fieldName x = modifyIORef' (traceFields trace) (H.insert fieldName $ toJSON x)

addSpanField :: (MonadIO m, ToJSON a) => Span -> Text -> a -> m ()
addSpanField EmptySpan _ _ = pure ()
addSpanField Span{..} fieldName x = do
  modifyIORef' fields (H.insert fieldName $ toJSON x)

addTraceFields :: (MonadIO m) => Trace -> HashMap Text Value -> m ()
addTraceFields trace fs = modifyIORef' (traceFields trace) (<> fs)

addSpanFields :: (MonadIO m) => Span -> HashMap Text Value -> m ()
addSpanFields EmptySpan _ = pure ()
addSpanFields Span{..} fs = modifyIORef' fields (<> fs)

data Link = Link
  { linkSpanId :: SpanId
  , linkTraceId :: TraceId
  }

addLink :: (MonadIO m, MonadReader r m, HasTracer r) => Trace -> ServiceName -> SpanId -> Text -> Link -> HashMap Text Value -> m ()
addLink t svc pid n Link{..} fs = do
  _id <- view tracerL >>= \Tracer{..} -> liftIO $ generateSpanId tracerIdGenerator
  liftIO $ do
    ts <- now
    -- putStrLn ("Starting span: " ++ show _id ++ " at " ++ show ts)
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
    pure ()

addEvent :: (MonadIO m, MonadReader r m, HasTracer r) => Trace -> ServiceName -> SpanId -> Text -> HashMap Text Value -> m ()
addEvent t svc pid n fs = do
  _id <- view tracerL >>= \Tracer{..} -> liftIO $ generateSpanId tracerIdGenerator
  liftIO $ do
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
    pure ()