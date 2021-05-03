{-# LANGUAGE NumericUnderscores #-}
module Honeycomb.API.Markers 
  ( Marker(..)
  , emptyMarker
  , ExistingMarker(..)
  , createMarker
  ) where

import Chronos
import Data.Text (Text)
-- import Honeycomb.Client
import Network.HTTP.Simple
import Data.Aeson
import Honeycomb.Client.Internal
import Honeycomb.Types
import Control.Lens (view)
import Data.Int

data Marker = Marker
  { startTime :: Maybe Time
  , endTime :: Maybe Time
  , message :: Maybe Text
  , markerType :: Maybe Text
  , url :: Maybe Text
  } deriving (Show, Eq)

emptyMarker :: Marker
emptyMarker = Marker
  { startTime = Nothing
  , endTime = Nothing
  , message = Nothing
  , markerType = Nothing
  , url = Nothing
  }

getSeconds :: Time -> Int64
getSeconds = (`div` 1_000_000_000) . getTime

fromSeconds :: Int64 -> Time
fromSeconds = Time . (* 1_000_000_000)

instance ToJSON Marker where
  toJSON Marker{..} = object
    [ "start_time" .= (getSeconds <$> startTime)
    , "end_time" .= (getSeconds <$> endTime)
    , "message" .= message
    , "type" .= markerType
    , "url" .= url
    ]

instance FromJSON Marker where
  parseJSON = withObject "Marker" $ \o ->
    Marker <$> 
    (fmap fromSeconds <$> (o .:? "start_time")) <*> 
    (fmap fromSeconds <$> (o .:? "end_time")) <*> 
    o .:? "message" <*> 
    o .:? "type" <*> 
    o .:? "url"

newtype MarkerId = MarkerId { fromMarkerId :: Text }
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

data ExistingMarker = ExistingMarker
  { id :: MarkerId
  , createdAt :: Text -- TODO current chronos version used in dev doesn't have Datetime FromJSON instance
  , updatedAt :: Text -- TODO current chronos version used in dev doesn't have Datetime FromJSON instance
  , color :: Maybe Text
  , marker :: Marker
  } deriving (Show, Eq)

instance FromJSON ExistingMarker where
  parseJSON x = existing x
    where
      existing = withObject "ExistingMarker" $ \o ->
        ExistingMarker <$> o .: "id" <*> o .: "created_at" <*> o .: "updated_at" <*> o .:? "color" <*> parseJSON x

-- TODO improve error handling
createMarker :: MonadHoneycomb env m => Marker -> m ExistingMarker
createMarker m = do
  c <- view honeycombClientL
  getResponseBody <$> post httpJSON ["1", "markers", fromDatasetName $ defaultDataset $ clientConfig c] [] m
-- updateMarker :: Client -> Marker
-- deleteMarker :: Client -> Marker
-- listAllMarkers :: Client -> Marker