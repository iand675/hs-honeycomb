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

newTrace :: (MonadIO m, MonadReader r m, HasTraceConfig r) => m Trace
newTrace =
  Trace
    <$> view (traceConfigL . honeycombClientL)
    <*> (view traceConfigL >>= \TraceConfig{..} -> liftIO $ generateTraceId traceIdGenerator)
    <*> liftIO (newIORef Nothing)
    <*> liftIO (newIORef mempty)
    <*> liftIO (newIORef mempty)
    <*> view traceConfigL

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
            (serviceNameField, String service),
            (durationField, toJSON (max (getTimespan (width (TimeInterval startT endT)) `div` 1_000_000) 1)),
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
  (MonadIO m, MonadReader r m, HasTraceConfig r) =>
  Trace ->
  ServiceName -> -- Service
  Maybe SpanId -> -- Parent ID
  Text -> -- Name
  m Span
newSpan t (ServiceName svc) pid n = do
  _id <- view traceConfigL >>= \TraceConfig{..} -> liftIO $ generateSpanId traceIdGenerator
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
  (MonadUnliftIO m, MonadReader r m, HasTraceConfig r, Exception e) =>
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

addField :: (MonadIO m, ToJSON a) => Span -> Text -> a -> m ()
addField EmptySpan _ _ = pure ()
addField Span{..} fieldName x = modifyIORef' fields (H.insert fieldName $ toJSON x)

addTraceFields :: (MonadIO m) => Trace -> HashMap Text Value -> m ()
addTraceFields trace fs = modifyIORef' (traceFields trace) (<> fs)

addFields :: (MonadIO m) => Span -> HashMap Text Value -> m ()
addFields EmptySpan _ = pure ()
addFields Span{..} fs = modifyIORef' fields (<> fs)