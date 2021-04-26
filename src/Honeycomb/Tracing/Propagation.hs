{-# LANGUAGE ExistentialQuantification #-}
module Honeycomb.Tracing.Propagation where
import Data.Aeson ( Value )
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Network.HTTP.Types
import Honeycomb.Tracing.Ids.TraceIdProvider

data PropagationContext
  = EmptyContext
  | Context 
      { parentId :: SpanId
      , traceId :: TraceId
      , dataset :: Maybe Text
      , traceFields :: HashMap Text Value 
      }

data ConcreteCodec a = PropagationCodec a => ConcreteCodec
  { codec :: a
  }

class PropagationCodec a where
  getName :: a -> Text
  decode :: a -> [Header] -> PropagationContext
  encode :: PropagationContext -> Maybe a

instance PropagationCodec () where
  getName () = "()"
  decode () _ = EmptyContext
  encode _ = Nothing