{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Tracing.Propagation.V1PropagationCodec where

import qualified Data.ByteString.Char8 as C
import Honeycomb.Tracing.Propagation
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (fromMaybe)
import Honeycomb.Tracing.Ids.TraceIdProvider
import Data.ByteArray.Encoding
import Data.Aeson (decodeStrict')
import Data.Bifunctor (second)

data V1PropagationCodec = V1PropagationCodec 

instance PropagationCodec V1PropagationCodec where
  getName _ = "V1PropagationCodec"
  decode _ hs = case lookup "x-honeycomb-trace" hs of
    Nothing -> EmptyContext
    Just str -> case second C.tail $ C.break (== ';') str of
      ("1", str') -> case C.split ',' str' of
        -- TODO need better tupling logic here
        rawEntries -> case map (second C.tail . C.break (== '=')) rawEntries of
          entries -> 
            fromMaybe EmptyContext $ do
              traceId <- TraceId . decodeUtf8 <$> lookup "trace_id" entries
              parentId <- SpanId . decodeUtf8 <$> lookup "span_id" entries
              let dataset = decodeUtf8 <$> lookup "dataset" entries
              context <- lookup "context" entries
              traceFields <- decodeStrict' =<< case convertFromBase Base64 context of
                Left _ -> Nothing
                Right ok -> Just ok
              pure Context{..}
      (_, _) -> EmptyContext
  encode _ = error "v1PropagationCodec"