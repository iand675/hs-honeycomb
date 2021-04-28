{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.API.Types where

import Chronos
    ( builder_YmdHMS,
      timeToDatetime,
      w3c,
      Datetime,
      SubsecondPrecision(SubsecondPrecisionFixed),
      Time )
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Data.Word

data Event = Event
  { eventSampleRate :: !(Maybe Word64)
  , eventTimestamp :: !(Maybe Time)
  , eventData :: !Object
  }

instance ToJSON Event where
  toJSON Event{..} = object $ ("data" .= eventData) : catMaybes
    [ (\x -> "time" .= encodeRFC3339 (timeToDatetime x)) <$> eventTimestamp
    , ("samplerate" .=) <$> eventSampleRate
    ]
  toEncoding Event{..} = pairs $ mconcat
    [ "data" .= eventData
    , maybe mempty (\x -> "time" .= encodeRFC3339 (timeToDatetime x)) eventTimestamp
    , maybe mempty ("samplerate" .=) eventSampleRate
    ]

-- | Construct a 'Text' 'TB.Builder' corresponding to the ISO-8601
--   encoding of the given 'Datetime'.
builderRFC3339 :: Datetime -> TB.Builder
builderRFC3339 dt = builder_YmdHMS (SubsecondPrecisionFixed 8) w3c dt <> "Z"

-- | Construct 'Text' corresponding to the ISO-8601
--   encoding of the given 'Datetime'.
encodeRFC3339 :: Datetime -> Text
encodeRFC3339 = LT.toStrict . TB.toLazyText . builderRFC3339
