{-# LANGUAGE DerivingStrategies #-}
module Honeycomb.Tracing.Ids.TraceIdProvider where
import Data.Text (Text)
import Data.Hashable
import Data.Aeson

newtype TraceId = TraceId { fromTraceId :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON)
newtype SpanId = SpanId { fromSpanId :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON)

-- | Interface that produces IDs for traces and spans.
class TraceIdProvider a where
  generateTraceId :: a -> IO TraceId
  generateSpanId :: a -> IO SpanId