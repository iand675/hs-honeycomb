module Honeycomb.Tracing.Propagation.W3CPropagationCodec 
  ( w3cPropagationCodec
  ) where
import Control.Lens hiding (Context)
import Honeycomb.Tracing.Propagation
import Network.HTTP.Types ( Header, HeaderName )
import Data.Text (Text)
import Control.Monad (guard)

parentHeader :: HeaderName
parentHeader = "traceparent"

stateHeader :: HeaderName
stateHeader = "tracestate"

segmentSplitter :: Char
segmentSplitter = '-'

headerLength :: Int
headerLength = 55

w3cPropagationCodec :: PropagationCodec [Header]
w3cPropagationCodec = CodecBuilder 
  { name = id -- TODO w3c
  , codec = undefined
  }

{-
  , codec = prism' encode decode
  }
  where
    encode Context{..} = 
      [ (parentHeader, _)
      , (stateHeader, _)
      ]
    decode hs = do
      h <- lookup parentHeader hs
      guard (length h == headerLength)
      Nothing
-}