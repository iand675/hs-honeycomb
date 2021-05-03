module Honeycomb.Tracing.Ids.W3CTraceIdProvider 
  ( W3CIdGenerator
  , newW3CIdGenerator
  , mkW3CIdGenerator
  ) where

import Data.ByteArray.Encoding
import Data.Text.Encoding ( decodeUtf8 )
import Honeycomb.Tracing.Ids.TraceIdProvider
import System.Random.MWC
import System.Random.Stateful
import Control.Monad.IO.Class

newtype W3CIdGenerator = W3CIdGenerator GenIO

mkW3CIdGenerator :: GenIO -> W3CIdGenerator
mkW3CIdGenerator = W3CIdGenerator

newW3CIdGenerator :: MonadIO m => m W3CIdGenerator
newW3CIdGenerator = W3CIdGenerator <$> liftIO create

traceIdByteLength :: Int
traceIdByteLength = 16

spanIdByteLength :: Int
spanIdByteLength = 8

-- TODO could probably ifdef this to support older random versions
instance TraceIdProvider W3CIdGenerator where
  generateTraceId (W3CIdGenerator st) = do
    bs <- uniformByteStringM traceIdByteLength st
    pure $ TraceId $ decodeUtf8 $ convertToBase Base16 bs

  generateSpanId (W3CIdGenerator st) = do
    bs <- uniformByteStringM spanIdByteLength st
    pure $ SpanId $ decodeUtf8 $ convertToBase Base16 bs
  
-- TODO format validation