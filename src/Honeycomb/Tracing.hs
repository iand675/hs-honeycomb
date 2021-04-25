{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Text (Text)
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

data Tracer = forall sampler idGenerator. (Sampler IO sampler, TraceIdProvider idGenerator) =>
  Tracer
    { tracerHoneycombClient :: HoneycombClient
    , tracerServiceName :: ServiceName
    , tracerPropagationCodecs :: [ConcreteCodec ByteString]
    , tracerSampler :: sampler
    , tracerIdGenerator :: idGenerator
    }

{-
fields:

"name" -- function name
"service_name" -- the name
"duration_ms" -- how much time the span took
"trace.span_id" -- unique id for the span
"trace.trace_id" -- unique id for the trace
"trace.parent_id" -- The ID of this span’s parent span, the call location the current span was called from
-}

-- TODO propogation

newtype ServiceName = ServiceName Text
  deriving (Show, Eq, Ord, IsString, ToJSON, FromJSON, Hashable)

class HasServiceName env where
  serviceNameL :: Lens' env ServiceName

instance HasServiceName ServiceName where
  serviceNameL = lens id (\_ new -> new)

instance HasServiceName Tracer where
  serviceNameL = lens tracerServiceName (\c s -> c { tracerServiceName = s })

class HasTrace env where
  traceL :: Lens' env Trace

instance HasTrace Trace where
  traceL = lens id (\_ new -> new)

instance HasTrace Span where
  traceL = lens trace (\s new -> s { trace = new })

class HasSpan env where
  spanL :: Lens' env Span

instance HasSpan Span where
  spanL = lens id (\_ new -> new)

data SpanErrorHandler = forall e. Exception e => SpanErrorHandler (Span -> e -> IO ())

noOpSpanErrorHandler :: SpanErrorHandler
noOpSpanErrorHandler = SpanErrorHandler $ \_ (_ :: SomeException) -> pure ()

class HasSpanErrorHandler env where
  spanErrorHandlerL :: Lens' env SpanErrorHandler

instance HasSpanErrorHandler SpanErrorHandler where
  spanErrorHandlerL = lens id (\_ new -> new)

instance HasHoneycombClient Tracer where
  honeycombClientL = lens tracerHoneycombClient (\t c -> t { tracerHoneycombClient = c })

class HasTracer env where
  tracerL :: Lens' env Tracer

instance HasTracer Tracer where
  tracerL = lens id (\_ new -> new)

data Trace = Trace
  { traceClient :: HoneycombClient,
    traceId :: TraceId,
    -- | We have to apply this to all head events, run sampling on them, do what with child spans...?
    traceSample :: IORef (Maybe Word64),
    -- | All spans
    traceSpans :: IORef (HashMap SpanId Span),
    -- | Fields that are added to all spans on completion
    traceFields :: IORef (HashMap Text Value),
    traceTracer :: Tracer
  }

instance Show Trace where
  show Trace{..} = "Trace {traceId = " ++ show traceId ++ "}"

instance HasTracer Trace where
  tracerL = lens traceTracer (\t c -> t { traceTracer = c })

data Span
  = Span
    { spanId :: SpanId,
      name :: Text,
      service :: ServiceName,
      startTime :: IORef Time,
      endTime :: IORef (Maybe Time),
      parentSpan :: Maybe SpanId,
      trace :: Trace,
      fields :: IORef (HashMap Text Value)
    }
  | EmptySpan

instance Show Span where
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
