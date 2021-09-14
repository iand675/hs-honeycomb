{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Honeycomb.Tracing.Propagation 
  ( PropagationContext(..)
  , encode
  , CodecBuilder(..)
  , HasCodecs(..)
  , PropagationCodec
  , HTTPCodec -- Perhaps the most common means of propagation
  , getContextFromCodecs
  ) where
import Data.Aeson ( Value )
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Network.HTTP.Types
import Honeycomb.Tracing.Ids.TraceIdProvider
import Honeycomb.Types (DatasetName)
import Prelude hiding (id, (.))
import Control.Category
import Control.Lens hiding (Context)
import Data.Foldable (asum)

data PropagationContext
  = Context 
      { propagatedParentId :: SpanId
      , propagatedTraceId :: TraceId
      , propagatedDataset :: Maybe DatasetName
      , propagatedTraceFields :: HashMap Text Value 
      } deriving (Show, Eq, Ord)


type DList a = [a] -> [a]

data CodecBuilder a b = CodecBuilder
  { name :: DList Text
  , codec :: APrism' a b
  }

type PropagationCodec serializedFormat = CodecBuilder serializedFormat PropagationContext

instance Category CodecBuilder where
  id = CodecBuilder id id
  (CodecBuilder ln lc) . (CodecBuilder rn rc) = CodecBuilder (ln . rn) (clonePrism rc . clonePrism lc)

type HTTPCodec = PropagationCodec [Header]

class HasCodecs env serializedFormat where
  codecsL :: Lens' env [PropagationCodec serializedFormat]

getContextFromCodecs :: [PropagationCodec serializedFormat] -> serializedFormat -> Maybe PropagationContext
getContextFromCodecs cs serialized = asum $ map (\x -> serialized ^? clonePrism (codec x)) cs

encode :: PropagationCodec serializedFormat -> PropagationContext -> serializedFormat
encode (CodecBuilder _ codec) = review (clonePrism codec)