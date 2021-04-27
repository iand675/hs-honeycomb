{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Honeycomb.Tracing.Instrumentation.Persistent where

import Conduit
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.Acquire.Internal
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.Typeable
import Database.Persist.Sql.Types.Internal
import Honeycomb.Tracing
import Honeycomb.Tracing.Fields
import qualified Honeycomb.Tracing.Raw as Raw
import UnliftIO
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Lens.Micro.Mtl (view)
import Honeycomb.Tracing.Monad (MonadTrace(..), askTrace, askSpan)
import Control.Monad.Morph
import Data.Maybe
import Database.Persist (BackendCompatible)
import qualified UnliftIO.Exception as UE
import Database.Persist.Sql (getStmtConn)
import Database.Persist.Class (projectBackend)

instance {-# OVERLAPS #-} (MonadTrace m, MonadUnliftIO m) => MonadTrace (ReaderT SqlBackend m) where
  askTraceContext = lift askTraceContext
  localTraceContext f = hoist (localTraceContext f)
  spanning n m = do
    conn <- ask
    lift $ spanning n $ do
      rewrapped <- rewrapConnection conn
      runReaderT m rewrapped

-- TODO
-- instance (MonadTrace m, MonadUnliftIO m) => MonadTrace (ReaderT SqlReadBackend m)
-- instance (MonadTrace m, MonadUnliftIO m) => MonadTrace (ReaderT SqlWriteBackend m)

annotateBasics :: MonadIO m => MutableSpan -> SqlBackend -> m ()
annotateBasics s conn = Raw.addSpanFields s $ H.fromList
  [ (packageField, String "persistent/esqueleto")
  , (serviceNameField, String $ connRDBMS conn)
  , (typeField, String "db")
  ]

-- TODO, so many variations on running the ReaderT...
runSqlPool :: forall backend m a. (MonadUnliftIO m, MonadTrace m, BackendCompatible SqlBackend backend, BaseBackend backend ~ SqlBackend, IsPersistBackend backend)
  => ReaderT backend m a
  -> Pool backend
  -> m a
runSqlPool r pconn = rawRunSqlPool r pconn Nothing

-- runSqlPoolWithIsolation
-- runSqlPoolNoTransaction
-- runSqlPoolWithHooks

rawRunSqlPool
    :: forall backend m a. (MonadUnliftIO m, MonadTrace m, BackendCompatible SqlBackend backend, IsPersistBackend backend, BaseBackend backend ~ SqlBackend)
    => ReaderT backend m a -> Pool backend -> Maybe IsolationLevel -> m a
rawRunSqlPool r pconn mi =
    runSqlPoolWithHooks r pconn mi before after onException
  where
    before conn = do
        let sqlBackend = projectBackend conn
        let getter = getStmtConn sqlBackend
        liftIO $ connBegin sqlBackend getter mi
    after conn = do
        let sqlBackend = projectBackend conn
        let getter = getStmtConn sqlBackend
        liftIO $ connCommit sqlBackend getter
    onException conn _ = do
        let sqlBackend = projectBackend conn
        let getter = getStmtConn sqlBackend
        liftIO $ connRollback sqlBackend getter

-- TODO this is 98% copy pasta from persistent. See if we could upstream a variant that supports our needs
-- TODO Matt Parsons does not like IsPersistBackend, so this is a bit regressive. Figure out how to convert SqlBackend -> (SqlWriteBackend | SqlReadBackend) in a better way.
runSqlPoolWithHooks
    :: forall backend m a before after onException. (MonadTrace m, MonadUnliftIO m, BackendCompatible SqlBackend backend, IsPersistBackend backend, BaseBackend backend ~ SqlBackend)
    => ReaderT backend m a
    -> Pool backend
    -> Maybe IsolationLevel
    -> (backend -> m before)
    -- ^ Run this action immediately before the action is performed.
    -> (backend -> m after)
    -- ^ Run this action immediately after the action is completed.
    -> (backend -> UE.SomeException -> m onException)
    -- ^ This action is performed when an exception is received. The
    -- exception is provided as a convenience - it is rethrown once this
    -- cleanup function is complete.
    -> m a
runSqlPoolWithHooks r pconn i before after onException =
    withRunInIO $ \runInIO ->
    withResource pconn $ \baseConn -> do
      conn <- runInIO $ wrapConnection (projectBackend baseConn)
      UE.mask $ \restore -> do
          _ <- restore $ runInIO $ before $ mkPersistBackend conn
          a <- restore (runInIO (runReaderT r $ mkPersistBackend conn))
              `UE.catchAny` \e -> do
                  _ <- restore $ runInIO $ onException (mkPersistBackend conn) e
                  UE.throwIO e
          _ <- restore $ runInIO $ after (mkPersistBackend conn)
          pure a






-- $ Connection wrapping utilities

rewrapConnection :: (MonadTrace m, MonadUnliftIO m) => SqlBackend -> m SqlBackend
rewrapConnection conn = do
  let baseConn = fromMaybe conn $ connUnwrappedConn conn
  wrapConnection baseConn

wrapConnection :: (MonadTrace m, MonadUnliftIO m) => SqlBackend -> m SqlBackend
wrapConnection conn = do
  tc@TraceContext{..} <- askTraceContext
  m <- askUnliftIO
  pure $ conn
    { connBegin = wrappedBegin tc m
    , connCommit = wrappedCommit tc m
    , connRollback = wrappedRollback tc m
    , connUnwrappedConn = Just $ fromMaybe conn $ connUnwrappedConn conn
    , connStatementMiddleware = \t stmt -> do
        pure $ stmt
          { stmtExecute = \ps -> unliftIO m $ do
            Raw.spanning
              tcTracer
              (trace tcSpan)
              tcSvc
              (pure $ spanId tcSpan)
              "db.query"
              [SpanErrorAnnotator $ \child (e :: SomeException) -> do
                Raw.addSpanField child databaseError $ show $ typeOf e
                Raw.addSpanField child databaseErrorDetails $ show e
              ]
              (\child -> do
                annotateBasics child conn
                Raw.addSpanField child databaseQueryField t
                Raw.addSpanField child databaseQueryParametersField $ show ps
                liftIO $ stmtExecute stmt ps
              )

          , stmtQuery = \ps -> do
              child <- mkAcquire (unliftIO m $ Raw.newSpan
                tcTracer
                (trace tcSpan)
                tcSvc
                (pure $ spanId tcSpan)
                "db.query") Raw.closeSpan
              annotateBasics child conn
              Raw.addSpanField child databaseQueryField t
              Raw.addSpanField child databaseQueryParametersField $ show ps

              case stmtQuery stmt ps of
                Acquire stmtQueryAcquireF -> Acquire $ \f -> handle (queryErrorHandler child) (stmtQueryAcquireF f)
          }
    }
  where
    queryErrorHandler child (e :: SomeException) = case e of
      SomeException e' -> do
        Raw.addSpanField child databaseError $ show $ typeOf e'
        Raw.addSpanField child databaseErrorDetails $ show e'
        throwIO e'

    wrappedBegin TraceContext{..} m preparer iso = unliftIO m $ do
      Raw.spanning
        tcTracer
        (trace tcSpan)
        tcSvc
        (pure $ spanId tcSpan)
        "db.transaction.begin"
        [SpanErrorAnnotator $ \child (e :: SomeException) -> do
          Raw.addSpanField child databaseError $ show $ typeOf e
          Raw.addSpanField child databaseErrorDetails $ show e
        ]
        (\child -> liftIO $ do
          annotateBasics child conn
          connBegin conn preparer iso
        )
    wrappedCommit TraceContext{..} m preparer = unliftIO m $ do
      Raw.spanning
        tcTracer
        (trace tcSpan)
        tcSvc
        (pure $ spanId tcSpan)
        "db.transaction.commit"
        [SpanErrorAnnotator $ \child (e :: SomeException) -> do
          Raw.addSpanField child databaseError $ show $ typeOf e
          Raw.addSpanField child databaseErrorDetails $ show e
        ]
        (\child -> liftIO $ do
          annotateBasics child conn
          connCommit conn preparer
        )
    wrappedRollback TraceContext{..} m preparer = unliftIO m $ do
      Raw.spanning
        tcTracer
        (trace tcSpan)
        tcSvc
        (pure $ spanId tcSpan)
        "db.transaction.rollback"
        [SpanErrorAnnotator $ \child (e :: SomeException) -> do
          Raw.addSpanField child databaseError $ show $ typeOf e
          Raw.addSpanField child databaseErrorDetails $ show e
        ]
        (\child -> liftIO $ do
          annotateBasics child conn
          connRollback conn preparer
        )

