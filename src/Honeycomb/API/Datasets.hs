module Honeycomb.API.Datasets where

import Data.Text (Text)
-- import Data.Vector (Vector)
-- import Honeycomb.Client
import Honeycomb.Types

newtype NewDataset = NewDataset
  { newDatasetName :: Text
  }

data Dataset = Dataset
  { datasetName :: Text
  , datasetSlug :: DatasetName
  }

{-
createDataset :: Client -> NewDataset -> m Dataset
createDataset = post c [] ["1", "datasets"]

getDataset :: Client -> DatasetName -> m (Maybe Dataset)
getDataset c (DatasetName d) = get c [] ["1", "datasets", d]

getAllDatasets :: Client -> m (Vector Dataset)
getAllDatasets c = get c [] ["1", "datasets"]
-}