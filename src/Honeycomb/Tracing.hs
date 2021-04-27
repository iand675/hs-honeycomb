{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Honeycomb.Tracing where

import Chronos
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text, empty)
import Data.Word
import Honeycomb.Client
import Honeycomb.Tracing.Fields
import Honeycomb.Tracing.Propagation hiding (PropagationContext(..))
import UnliftIO
import Honeycomb.Tracing.Sampling
import Honeycomb.Tracing.Ids.TraceIdProvider
    ( SpanId, TraceId, TraceIdProvider(..) )
import Honeycomb.Client.Internal
import Lens.Micro
import Data.String (IsString)
import Lens.Micro.Mtl (view)
import Data.Hashable (Hashable)
import Honeycomb.Types
import Control.Monad.Primitive

data Tracer = forall sampler idGenerator. (Sampler IO sampler, TraceIdProvider idGenerator) =>
  Tracer
    { tracerHoneycombClient :: HoneycombClient
    , tracerServiceName :: ServiceName
    , tracerPropagationCodecs :: [ConcreteCodec]
    , tracerSampler :: sampler
    , tracerIdGenerator :: idGenerator
    , tracerSpanPostProcessor :: Maybe (ImmutableSpan -> (Maybe Word64, HashMap Text Value))
    }

data Mutability = Mutable | Immutable
type family MutableField (mut :: Mutability) a where
  MutableField 'Mutable a = IORef a
  MutableField 'Immutable a = a

-- TODO there's a glimmer of something more broadly useful here.
class Transient (t :: Mutability -> *) where
  freeze :: MonadIO m => t 'Mutable -> m (t 'Immutable)
  thaw :: MonadIO m => t 'Immutable -> m (t 'Mutable)

newtype ServiceName = ServiceName Text
  deriving (Show, Eq, Ord, IsString, ToJSON, FromJSON, Hashable)

class HasServiceName env where
  serviceNameL :: Lens' env ServiceName

instance HasServiceName ServiceName where
  serviceNameL = lens id (\_ new -> new)

instance HasServiceName Tracer where
  serviceNameL = lens tracerServiceName (\c s -> c { tracerServiceName = s })

class HasTrace env where
  traceL :: Lens' env MutableTrace

instance HasTrace MutableTrace where
  traceL = lens id (\_ new -> new)

class HasSpan env where
  spanL :: Lens' env MutableSpan

instance HasSpan MutableSpan where
  spanL = lens id (\_ new -> new)

data SpanErrorAnnotator = forall e. Exception e => SpanErrorAnnotator (MutableSpan -> e -> IO ())

recordAnchoredErrorHandler :: MutableSpan -> SpanErrorAnnotator
recordAnchoredErrorHandler s = case s of
  EmptySpan -> recordBasicErrorHandler
  Span{..} -> SpanErrorAnnotator $ \_ (e :: SomeException) -> do
    modifyIORef' fields (H.insert "error" $ toJSON $ show e)

recordBasicErrorHandler :: SpanErrorAnnotator
recordBasicErrorHandler = SpanErrorAnnotator $ \innerSpan (e :: SomeException) -> do
  -- TODO use Raw.addSpanField for consistency
  case innerSpan of
    EmptySpan -> pure () -- WAT
    Span{..} -> modifyIORef' fields (H.insert "error" $ toJSON $ show e)

class HasSpanErrorAnnotators env where
  spanErrorHandlerL :: Lens' env [SpanErrorAnnotator]

instance HasSpanErrorAnnotators [SpanErrorAnnotator] where
  spanErrorHandlerL = lens id (\_ new -> new)

instance HasHoneycombClient Tracer where
  honeycombClientL = lens tracerHoneycombClient (\t c -> t { tracerHoneycombClient = c })

class HasTracer env where
  tracerL :: Lens' env Tracer

instance HasTracer Tracer where
  tracerL = lens id (\_ new -> new)

data Trace (mut :: Mutability) = Trace
  { traceId :: TraceId,
    traceServiceName :: ServiceName,
    -- ^ We have to apply this to all head events, run sampling on the head, use the same value on all spans.
    traceSample :: MutableField mut (Maybe Word64),
    traceSpans :: MutableField mut (HashMap SpanId (Span mut)),
    -- ^ All spans associated with the trace locally
    traceFields :: MutableField mut (HashMap Text Value),
    -- ^ Fields that are added to all spans on completion
    traceDataset :: Maybe DatasetName
    -- ^ Used to propagate within the appropriate dataset
  }

type MutableTrace = Trace 'Mutable
type ImmutableTrace = Trace 'Immutable

instance Transient Trace where
  freeze Trace{..} = 
    Trace traceId traceServiceName
      <$> readIORef traceSample
      <*> (readIORef traceSpans >>= mapM freeze)
      <*> readIORef traceFields
      <*> pure traceDataset
  thaw Trace{..} =
    Trace traceId traceServiceName
      <$> newIORef traceSample
      <*> (mapM thaw traceSpans >>= newIORef)
      <*> newIORef traceFields
      <*> pure traceDataset

instance Show (Trace 'Mutable) where
  show Trace{..} = "Trace {traceId = " ++ show traceId ++ "}"

deriving instance Show (Trace 'Immutable)

data Span (mut :: Mutability)
  = Span
    { spanId :: SpanId,
      name :: Text,
      service :: ServiceName,
      startTime :: MutableField mut Time,
      endTime :: MutableField mut (Maybe Time),
      parentSpan :: Maybe SpanId,
      trace :: MutableTrace, 
      -- ^ This being always mutable is intentional, as it would otherwise force
      -- the freeze instance of span to create immutable copies of all spans in
      -- the trace
      fields :: MutableField mut (HashMap Text Value)
    }
  | EmptySpan

type MutableSpan = Span 'Mutable
type ImmutableSpan = Span 'Immutable

instance Show MutableSpan where
  show Span{..} = 
    "Span {spanId = " ++ 
    show spanId ++ 
    ", name = " ++ 
    show name ++ 
    ", service = " ++ 
    show service ++
    ", parentSpan = " ++
    show parentSpan ++
    ", trace = " ++ 
    show trace ++ 
    "}"
  show EmptySpan = "EmptySpan"

deriving instance Show ImmutableSpan

instance HasTrace MutableSpan where
  traceL = lens trace (\s t -> s { trace = t })

instance Transient Span where
  freeze EmptySpan = pure EmptySpan
  freeze Span{..} =
    Span spanId name service 
      <$> readIORef startTime
      <*> readIORef endTime
      <*> pure parentSpan
      <*> pure trace
      <*> readIORef fields
  thaw EmptySpan = pure EmptySpan
  thaw Span{..} =
    Span spanId name service
      <$> newIORef startTime
      <*> newIORef endTime
      <*> pure parentSpan
      <*> pure trace
      <*> newIORef fields

-- | A data type that provides all of the necessary functionality to trace things
-- via 'spanning'
data TraceContext = TraceContext
  { tcSvc :: ServiceName
  , tcTracer :: Tracer
  , tcSpan :: MutableSpan
  , tcErrorAnnotators :: [SpanErrorAnnotator]
  }

instance HasServiceName TraceContext where
  serviceNameL = lens tcSvc (\c s -> c { tcSvc = s })
instance HasTracer TraceContext where
  tracerL = lens tcTracer (\c t -> c { tcTracer = t })
instance HasSpan TraceContext where
  spanL = lens tcSpan (\c s -> c { tcSpan = s })
instance HasSpanErrorAnnotators TraceContext where
  spanErrorHandlerL = lens tcErrorAnnotators (\c e -> c { tcErrorAnnotators = e })

class HasTraceContext a where
  traceContextL :: Lens' a TraceContext

instance HasTraceContext TraceContext where
  traceContextL = lens id (\_ new -> new)

asSimpleTraceContext :: (HasSpanErrorAnnotators env, HasServiceName env, HasTracer env, HasSpan env) => env -> TraceContext
asSimpleTraceContext x = TraceContext (x ^. serviceNameL) (x ^. tracerL) (x ^. spanL) (x ^. spanErrorHandlerL)
