{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Tracing.Propagation.AWSPropagationCodec where

import Data.String
import Honeycomb.Tracing.Propagation
import Network.HTTP.Types
-- import Honeycomb.Tracing.Propagation

-- Derived from
-- https://github.com/honeycombio/beeline-java/blob/main/beeline-core/src/main/java/io/honeycomb/beeline/tracing/propagation/AWSPropagationCodec.java
-- https://github.com/honeycombio/beeline-java/blob/main/beeline-core/src/test/java/io/honeycomb/beeline/tracing/propagation/AWSPropagationCodecTest.java

codecName :: IsString s => s
codecName = "aws"

awsTraceHeader :: IsString s => s
awsTraceHeader = "X-Amzn-Trace-Id"

segmentSeparator :: Char
segmentSeparator = ';'

kvSeparator :: Char
kvSeparator = '='

rootKey :: IsString s => s
rootKey = "root" 

parentKey :: IsString s => s
parentKey = "parent" 

selfKey :: IsString s => s
selfKey  = "self" 

awsPropagationCodec :: PropagationCodec [Header]
awsPropagationCodec = undefined
