module Honeycomb.API.Columns where

import Data.Text (Text)
-- import Honeycomb.Types

data ColumnType 
  = StringType
  | FloatType
  | IntegerType
  | BooleanType

newtype ColumnId = ColumnId Text

data ExistingColumn = ExistingColumn
  { columnId :: ColumnId
  , columnData :: Column
  }

data Column = Column
  { keyName :: Text 
  , hidden :: Bool
  , description :: Maybe Text
  , columnType :: Maybe ColumnType
  }

{-
createColumn :: DatasetName -> Column -> ExistingColumn
updateColumn :: DatasetName -> Column -> ExistingColumn
deleteColumn :: DatasetName -> ColumnId -> ExistingColumn
getColumn :: DatasetName -> ColumnId -> Maybe ExistingColumn
getColumnByKeyName :: DatasetName -> Text -> Maybe ExistingColumn
listAllColumns :: DatasetName -> [ExistingColumn]
-}