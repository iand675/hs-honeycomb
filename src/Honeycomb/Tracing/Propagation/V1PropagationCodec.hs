{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Tracing.Propagation.V1PropagationCodec where

import qualified Data.ByteString.Char8 as C
import Honeycomb.Tracing.Propagation
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe, catMaybes)
import Honeycomb.Tracing.Ids.TraceIdProvider
import Data.ByteArray.Encoding
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Aeson (decodeStrict')
import qualified Data.Aeson as A
import Data.Bifunctor (second)
import Honeycomb.Types
import Data.List (intersperse)

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
              propagatedTraceId <- TraceId . decodeUtf8 <$> lookup "trace_id" entries
              propagatedParentId <- SpanId . decodeUtf8 <$> lookup "span_id" entries
              let propagatedDataset = DatasetName . decodeUtf8 <$> lookup "dataset" entries
              let propagatedTraceFields = fromMaybe H.empty $ do
                    context <- lookup "context" entries
                    decodeStrict' =<< case convertFromBase Base64 context of
                      Left _ -> Nothing
                      Right ok -> Just ok
              pure Context{..}
      (_, _) -> EmptyContext
  encode _ EmptyContext = []
  encode _ Context{..} = [("x-honeycomb-trace", L.toStrict $ B.toLazyByteString encoded)]
    where
      encoded :: B.Builder
      encoded = B.shortByteString "1;" <> mconcat 
        ( intersperse (B.charUtf8 ',')
          [ B.shortByteString "trace_id=" <> B.byteString (encodeUtf8 (fromTraceId propagatedTraceId))
          , B.shortByteString "span_id=" <> B.byteString (encodeUtf8 (fromSpanId propagatedParentId))
          , maybe mempty (\ds -> B.shortByteString "dataset=" <> B.byteString (encodeUtf8 $ fromDatasetName ds)) propagatedDataset
          , if H.null propagatedTraceFields
            then mempty
            else B.shortByteString "context=" <> B.byteString (convertToBase Base64 $ L.toStrict (A.encode propagatedTraceFields))
          ]
        )
