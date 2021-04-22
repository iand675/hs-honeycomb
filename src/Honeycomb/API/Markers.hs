module Honeycomb.API.Markers where

import Chronos
import Data.Text (Text)
-- import Honeycomb.Client

data Marker = Marker
  { startTime :: Maybe Time
  , endTime :: Maybe Time
  , message :: Maybe Text
  , markerType :: Maybe Text
  , url :: Maybe Text
  }

{-
createMarker :: Client -> Marker
updateMarker :: Client -> Marker
deleteMarker :: Client -> Marker
listAllMarkers :: Client -> Marker
-}