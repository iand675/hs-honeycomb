{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Honeycomb.Tracing.Instrumentation.Persistent where

import Conduit
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.Acquire.Internal
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

rewrapConnection :: (MonadTrace m, MonadUnliftIO m) => SqlBackend -> m SqlBackend
rewrapConnection conn = do
  let baseConn = fromMaybe conn $ connUnwrappedConn conn
  wrapConnection baseConn

instance (MonadTrace m, MonadUnliftIO m) => MonadTrace (ReaderT SqlBackend m) where
  askTraceContext = lift askTraceContext
  localTraceContext f = hoist (localTraceContext f)
  spanning n m = do
    conn <- ask
    lift $ spanning n $ do
      rewrapped <- rewrapConnection conn
      runReaderT m rewrapped

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


annotateBasics :: MonadIO m => MutableSpan -> SqlBackend -> m ()
annotateBasics s conn = Raw.addSpanFields s $ H.fromList
  [ (packageField, String "persistent/esqueleto")
  , (serviceNameField, String $ connRDBMS conn)
  , (typeField, String "db")
  ]
