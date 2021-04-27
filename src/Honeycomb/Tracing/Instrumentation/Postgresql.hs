{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Tracing.Instrumentation.Postgresql where
import Database.PostgreSQL.Simple hiding
  ( connectPostgreSQL 
  , close
  , connect
  , query
  , query_
  , queryWith
  , queryWith_
  , fold
  , foldWithOptions
  , fold_
  , foldWithOptions_
  , forEach
  , forEach_
  , returning
  , foldWith
  , foldWithOptionsAndParser
  , foldWith_
  )
import Control.Monad.IO.Class
import qualified Database.PostgreSQL.Simple as Simple
import Honeycomb.Tracing.Monad
import Data.ByteString.Char8
import Database.PostgreSQL.Simple.FromRow
import Data.Int
import UnliftIO
import Honeycomb.Tracing.Fields
import Data.Text (Text)

connectPostgreSQL :: (MonadTrace m) => ByteString -> m Simple.Connection
connectPostgreSQL = spanning "connectPostgresql" . liftIO . Simple.connectPostgreSQL 

close :: (MonadTrace m) => Simple.Connection -> m ()
close ci = spanning "close" $ do
  annotateBasics
  liftIO $ Simple.close ci

connect :: (MonadTrace m) => Simple.ConnectInfo -> m Simple.Connection
connect ci = spanning "connect" $ do
  annotateBasics
  liftIO $ Simple.connect ci

query :: (MonadTrace m, Simple.ToRow q, Simple.FromRow r) => Simple.Connection -> Simple.Query -> q -> m [r]
query c q params = spanning "query" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.query c q params

query_ :: (MonadTrace m, FromRow r) => Connection -> Query -> m [r]
query_ c q = spanning "query_" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.query_ c q

queryWith :: (MonadTrace m, ToRow q) => RowParser r -> Connection -> Query -> q -> m [r]
queryWith p c q params = spanning "queryWith" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.queryWith p c q params

queryWith_ :: (MonadTrace m) => RowParser r -> Connection -> Query -> m [r]
queryWith_ r c q = spanning "queryWith_" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.queryWith_ r c q

fold :: (MonadUnliftIO m, MonadTrace m, FromRow row, ToRow params) => Connection -> Query -> params -> a -> (a -> row -> m a) -> m a
fold c q ps x f = spanning "fold" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do 
    Simple.fold c q ps x (\x' r -> runInIO $ f x' r)

foldWithOptions :: (MonadUnliftIO m, MonadTrace m, FromRow row, ToRow params) => FoldOptions -> Connection -> Query -> params -> a -> (a -> row -> m a) -> m a
foldWithOptions fopts c q ps x f = spanning "foldWithOptions" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.foldWithOptions fopts c q ps x (\x' r -> runInIO $ f x' r)

fold_ :: (MonadUnliftIO m, MonadTrace m, FromRow r) => Connection -> Query -> a -> (a -> r -> m a) -> m a
fold_ c q x f = spanning "fold_" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.fold_ c q x (\x' r -> runInIO $ f x' r)

foldWithOptions_ :: (MonadUnliftIO m, MonadTrace m, FromRow r) => FoldOptions -> Connection -> Query -> a -> (a -> r -> m a) -> m a
foldWithOptions_ fopts c q x f = spanning "foldWithOptions_" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.foldWithOptions_ fopts c q x (\x' r -> runInIO $ f x' r)

forEach :: (MonadUnliftIO m, MonadTrace m, ToRow q, FromRow r) => Connection -> Query -> q -> (r -> m ()) -> m ()
forEach c q r f = spanning "forEach" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.forEach c q r (runInIO . f)

forEach_ :: (MonadUnliftIO m, MonadTrace m, FromRow r) => Connection -> Query -> (r -> m ()) -> m ()
forEach_ c q f = spanning "forEach_" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.forEach_ c q (runInIO . f)

returning :: (MonadTrace m, ToRow q, FromRow r) => Connection -> Query -> [q] -> m [r]
returning c q rs = spanning "returning" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.returning c q rs

foldWith :: (MonadUnliftIO m, MonadTrace m, ToRow params) => RowParser row -> Connection -> Query -> params -> a -> (a -> row -> m a) -> m a
foldWith rp c q ps x f = spanning "foldWith" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.foldWith rp c q ps x (\x' r -> runInIO $ f x' r)

foldWithOptionsAndParser :: (MonadUnliftIO m, MonadTrace m, ToRow params) => FoldOptions -> RowParser row -> Connection -> Query -> params -> a -> (a -> row -> m a) -> m a
foldWithOptionsAndParser fopts rp c q params x f = spanning "foldWithOptionsAndParser" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.foldWithOptionsAndParser fopts rp c q params x (\x' r -> runInIO $ f x' r)

foldWith_ :: (MonadUnliftIO m, MonadTrace m) => RowParser r -> Connection -> Query -> a -> (a -> r -> m a) -> m a
foldWith_ rp c q x f = spanning "foldWith_" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.foldWith_ rp c q x (\x' r -> runInIO $ f x' r)

foldWithOptionsAndParser_ :: (MonadUnliftIO m, MonadTrace m) => FoldOptions -> RowParser r -> Connection -> Query -> a -> (a -> r -> m a) -> m a
foldWithOptionsAndParser_ fopts rp c q x f = spanning "foldWithOptionsAndParser_" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.foldWithOptionsAndParser_ fopts rp c q x (\x' r -> runInIO $ f x' r)

forEachWith :: (MonadUnliftIO m, MonadTrace m, ToRow q) => RowParser r -> Connection -> Query -> q -> (r -> m ()) -> m ()
forEachWith rp c q params f = spanning "forEachWith" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.forEachWith rp c q params (runInIO . f)

forEachWith_ :: (MonadUnliftIO m, MonadTrace m) => RowParser r -> Connection -> Query -> (r -> m ()) -> m ()
forEachWith_ rp c q f = spanning "forEachWith_" $ do
  annotateBasics
  annotateQuery q
  withRunInIO $ \runInIO -> do
    Simple.forEachWith_ rp c q (runInIO . f)

returningWith :: (MonadTrace m, ToRow q) => RowParser r -> Connection -> Query -> [q] -> m [r]
returningWith rp c q params = spanning "returningWith" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.returningWith rp c q params

execute :: (MonadTrace m, ToRow q) => Connection -> Query -> q -> m Int64
execute c q r = spanning "execute" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.execute c q r

execute_ :: (MonadTrace m) => Connection -> Query -> m Int64
execute_ c q = spanning "execute_" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.execute_ c q

executeMany :: (MonadTrace m, ToRow q) => Connection -> Query -> [q] -> m Int64
executeMany c q rs = spanning "executeMany" $ do
  annotateBasics
  annotateQuery q
  liftIO $ Simple.executeMany c q rs

withTransaction :: (MonadUnliftIO m, MonadTrace m) => Connection -> m a -> m a
withTransaction c m = spanning "withTransaction" $ do
  annotateBasics
  withRunInIO $ \runInIO -> 
    Simple.withTransaction c $ runInIO m

withSavepoint :: (MonadUnliftIO m, MonadTrace m) => Connection -> m a -> m a
withSavepoint c m = spanning "withSavepoint" $ do
  annotateBasics
  withRunInIO $ \runInIO ->
    Simple.withSavepoint c $ runInIO m

begin :: (MonadTrace m) => Connection -> m ()
begin c = spanning "begin" $ do
  annotateBasics
  liftIO $ Simple.begin c

commit :: (MonadTrace m) => Connection -> m ()
commit c = spanning "commit" $ do
  annotateBasics
  liftIO $ Simple.commit c

rollback :: (MonadTrace m) => Connection -> m ()
rollback c = spanning "rollback" $ do
  annotateBasics
  liftIO $ Simple.rollback c

formatMany :: (MonadTrace m, ToRow q) => Connection -> Query -> [q] -> m ByteString
formatMany c q rs = spanning "formatMany" $ do
  annotateBasics
  liftIO $ Simple.formatMany c q rs

formatQuery :: (MonadTrace m, ToRow q) => Connection -> Query -> q -> m ByteString
formatQuery c q r = spanning "formatQuery" $ do
  annotateBasics
  liftIO $ Simple.formatQuery c q r


-- Utils

annotateBasics :: MonadTrace m => m ()
annotateBasics = do
  addSpanField serviceNameField ("postgresql" :: Text)
  addSpanField packageField ("postgresql-simple" :: Text)

annotateQuery :: (MonadTrace m) => Query -> m ()
annotateQuery q = addSpanField databaseQueryField (show q)

-- annotateParams :: (MonadTrace env m) => q -> m ()
-- annotateParams q = addField databaseQueryField (show q)
