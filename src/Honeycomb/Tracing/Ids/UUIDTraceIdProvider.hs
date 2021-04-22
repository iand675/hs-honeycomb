module Honeycomb.Tracing.Ids.UUIDTraceIdProvider where
import Honeycomb.Tracing.Ids.TraceIdProvider

import Data.UUID
import Data.UUID.V4

data UUIDGenerator = UUIDGenerator

instance TraceIdProvider UUIDGenerator where
  generateTraceId _ = TraceId . toText <$> nextRandom
  generateSpanId _ = SpanId . toText <$> nextRandom

