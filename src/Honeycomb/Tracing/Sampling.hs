{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Tracing.Sampling where

import Crypto.Hash ( SHA256, hash, Digest )
import Data.ByteArray.Mapping ( toW64BE )
import Data.Memory.Endian ( BE(BE) )
import Data.Text.Encoding ( encodeUtf8 )
import Honeycomb.Tracing.Ids.TraceIdProvider ( TraceId(..) )

data Always = Always
data Never = Never

newtype Deterministic = Deterministic
  { sampleRate :: Int
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
  sample Deterministic{..} (TraceId txt) = if digestInt <= upperBound
    then pure $ fromIntegral sampleRate
    else pure 0
    where
      digestInt = fromIntegral digestW64
      (BE digestW64) = toW64BE (digest :: Digest SHA256) 0
      digest = hash (encodeUtf8 txt)
      upperBound = maxBound `div` sampleRate
