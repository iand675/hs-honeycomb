{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Tracing.Propagation.AWSPropagationCodec where

import Data.String
-- import Honeycomb.Tracing.Propagation

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

data AWSPropagationCodec

{-
instance PropagationCodec AWSPropagationCodec where
  getName = _
  decode = _
  encode = _
-} 