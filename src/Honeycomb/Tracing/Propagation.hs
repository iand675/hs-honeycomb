{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module Honeycomb.Tracing.Propagation where
import Data.Aeson ( Value )
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Network.HTTP.Types
import Honeycomb.Tracing.Ids.TraceIdProvider
import Honeycomb.Types (DatasetName)

data PropagationContext
  = EmptyContext
  | Context 
      { propagatedParentId :: SpanId
      , propagatedTraceId :: TraceId
      , propagatedDataset :: Maybe DatasetName
      , propagatedTraceFields :: HashMap Text Value 
      } deriving (Show, Eq, Ord)


-- TODO the header dependency here isn't ideal
class PropagationCodec a where
  getName :: a -> Text
  decode :: a -> [Header] -> PropagationContext
  encode :: a -> PropagationContext -> [Header]

data ConcreteCodec = forall a. PropagationCodec a => ConcreteCodec
  { codec :: a
  }

instance PropagationCodec ConcreteCodec where
  getName (ConcreteCodec c) = getName c
  decode (ConcreteCodec c) = decode c
  encode (ConcreteCodec c) = encode c

instance PropagationCodec () where
  getName () = "()"
  decode () _ = EmptyContext
  encode _ _ = []

isPropagated :: PropagationContext -> Bool
isPropagated EmptyContext = False
isPropagated Context{} = True

getContextFromCodecs :: PropagationCodec codec => [codec] -> [Header] -> PropagationContext
getContextFromCodecs cs hs = case filter isPropagated $ map (`decode` hs) cs of
  [] -> EmptyContext
  (c:_) -> c