{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Types 
  ( DatasetName(..)
  ) where
import Lens.Micro (Lens', lens)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word
import Data.String (IsString)

newtype DatasetName = DatasetName { fromDatasetName :: Text }
  deriving (Show, Eq, Ord, IsString)
