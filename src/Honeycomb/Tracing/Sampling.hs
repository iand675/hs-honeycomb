{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Tracing.Sampling where

import Crypto.Hash ( SHA256, hash, Digest )
import Data.Memory.Endian ( BE(BE) )
import Data.Text.Encoding ( encodeUtf8 )
import Honeycomb.Tracing.Ids.TraceIdProvider ( TraceId(..) )
import GHC.IO.Unsafe (unsafePerformIO)
import Data.ByteArray (withByteArray)
import Foreign.Storable ( peek )
import Data.Word (Word32)

data Always = Always
data Never = Never

newtype Deterministic = Deterministic
  { sampleRate :: Word32
  }

data ConcreteSampler = forall a. Sampler IO a => Sampler a

class Sampler m a where
  sample :: a -> TraceId -> m Word

instance Applicative m => Sampler m Always where
  sample _ _ = pure 1

instance Applicative m => Sampler m Never where
  sample _ _ = pure 0

instance Applicative m => Sampler m Deterministic where
  sample (Deterministic 1) t = sample Always t
  sample (Deterministic 0) t = sample Never t
  sample d t = pure $ dsample d t
    where
      dsample :: Deterministic -> TraceId -> Word
      dsample Deterministic{..} (TraceId txt) = 
        if digestW32 > upperBound then fromIntegral sampleRate else 0
        where
          (BE digestW32) = unsafePerformIO $ withByteArray (digest :: Digest SHA256) (\p -> peek p :: IO (BE Word32))
          digest = hash (encodeUtf8 txt)
          upperBound = maxBound `div` sampleRate
