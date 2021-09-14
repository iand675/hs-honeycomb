{-# LANGUAGE GADTs #-}
module Honeycomb.API.DerivedColumns where


import Data.Text (Text)
import Honeycomb.Types (DatasetName)
import Honeycomb.Client.Internal

newtype Expression = Expression Text

data DerivedColumn = DerivedColumn
  { alias :: Text
  , description :: Maybe Text
  , expression :: Expression 
  }

newtype DerivedColumnId = DerivedColumnId Text

data ExistingDerivedColumn = ExistingDerivedColumn 
  { derivedColumnId :: DerivedColumnId
  , derivedColumnDetails :: DerivedColumn
  }

{-
createDerivedColumn :: DatasetName -> DerivedColumn -> m ExistingDerivedColumn
createDerivedColumn = post $ error "TODO"

updateDerivedColumn :: DatasetName -> ExistingDerivedColumn -> m ExistingDerivedColumn
updateDerivedColumn = put $ error "TODO"

deleteDerivedColumn :: DatasetName -> DerivedColumnId -> m Bool
deleteDerivedColumn = delete $ error "TODO"

getOneDerivedColumn :: DatasetName -> DerivedColumnId -> m (Maybe ExistingDerivedColumn)
getOneDerivedColumn = get $ error "TODO"

getOneDerivedColumnByAlias :: DatasetName -> Text -> m (Maybe ExistingDerivedColumn)
getOneDerivedColumnByAlias = get $ error "TODO"

listAllDerivedColumns :: DatasetName -> m [ExistingDerivedColumn]
listAllDerivedColumns = get $ error "TODO"
-}

{-
data Expression a where
  ColumnName :: Text -> Expression ()
  Function :: Expression ()
  StringLit :: Text -> Expression Text
  NumLit :: Scientific -> Expression Scientific
  BoolLit :: Bool -> Expression Bool 
  NullLit :: Expression a


ifE :: Expression Bool -> Expression a -> Expression a
ifElseE :: Expression Bool -> Expression a -> Expression a -> Expression a
multiWayIfE :: Expression Bool -> NonEmpty (Expression a, Expression a) -> Expression a 

coalesce :: [Expression a] -> Expression a

lt :: Expression a -> Expression b -> Expression Bool
lte :: Expression a -> Expression b -> Expression Bool
gt :: Expression a -> Expression b -> Expression Bool
gte :: Expression a -> Expression b -> Expression Bool
equals :: Expression a -> Expression b -> Expression Bool
in_ :: Expression a -> [Expression b] -> Expression Bool
exists :: Expression a -> Expression Bool
not :: Expression Bool -> Expression Bool
and :: Expression Bool -> Expression Bool -> Expression Bool
or :: Expression
min
max
sum
sub
mul
log10
bucket

-- $ Cast operators
int
float
bool
string
concat
startsWith
contains
regMatch
regValue
regCount
unixTimestamp
eventTimestamp
ingestTimestamp
-}