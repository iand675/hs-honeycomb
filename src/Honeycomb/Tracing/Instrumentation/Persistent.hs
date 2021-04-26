{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Honeycomb.Tracing.Instrumentation.Persistent where

import Conduit
import Control.Monad.Reader (MonadReader)
import Data.Acquire.Internal
import Data.Text (Text)
import Data.Typeable
import Database.Persist.Sql.Types.Internal
import Honeycomb.Tracing
import Honeycomb.Tracing.Fields
import Honeycomb.Tracing.Raw
import UnliftIO
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Lens.Micro.Mtl (view)

wrapConnection :: (MonadUnliftIO m, MonadReader r m, HasTracer r) => MutableSpan -> SqlBackend -> m SqlBackend
wrapConnection EmptySpan conn = pure conn
wrapConnection Span{..} conn = do
  m <- askUnliftIO
  pure $ conn
    { connBegin = wrappedBegin m
    , connCommit = wrappedCommit m
    , connRollback = wrappedRollback m
    , connStatementMiddleware = \t stmt -> do
        pure $ stmt
          { stmtExecute = \ps -> unliftIO m $ do
            spanning
              trace
              (traceServiceName trace)
              (Just spanId)
              "db.query"
              (\child (e :: SomeException) -> do
                addSpanField child databaseError $ show $ typeOf e
                addSpanField child databaseErrorDetails $ show e
              )
              (\child -> do
                annotateBasics child conn
                addSpanField child databaseQueryField t
                addSpanField child databaseQueryParametersField $ show ps
                liftIO $ stmtExecute stmt ps
              )

          , stmtQuery = \ps -> do
              child <- mkAcquire (unliftIO m $ newSpan
                    trace
                    (traceServiceName trace)
                    (Just spanId)
                    "db.query") closeSpan
              annotateBasics child conn
              addSpanField child databaseQueryField t
              addSpanField child databaseQueryParametersField $ show ps

              case stmtQuery stmt ps of
                Acquire stmtQueryAcquireF -> Acquire $ \f -> handle (queryErrorHandler child) (stmtQueryAcquireF f)
          }
    }
  where
    queryErrorHandler child (e :: SomeException) = case e of
      SomeException e' -> do
        addSpanField child databaseError $ show $ typeOf e'
        addSpanField child databaseErrorDetails $ show e'
        throwIO e'

    wrappedBegin m preparer iso = unliftIO m $ do
      spanning
        trace
        (traceServiceName trace)
        (Just spanId)
        "db.transaction.begin"
        (\child (e :: SomeException) -> do
          addSpanField child databaseError $ show $ typeOf e
          addSpanField child databaseErrorDetails $ show e
        )
        (\child -> liftIO $ do
          annotateBasics child conn
          connBegin conn preparer iso
        )
    wrappedCommit m preparer = unliftIO m $ do
      spanning
        trace
        (traceServiceName trace)
        (Just spanId)
        "db.transaction.commit"
        (\child (e :: SomeException) -> do
          addSpanField child databaseError $ show $ typeOf e
          addSpanField child databaseErrorDetails $ show e
        )
        (\child -> liftIO $ do
          annotateBasics child conn
          connCommit conn preparer
        )
    wrappedRollback m preparer = unliftIO m $ do
      spanning
        trace
        (traceServiceName trace)
        (Just spanId)
        "db.transaction.rollback"
        (\child (e :: SomeException) -> do
          addSpanField child databaseError $ show $ typeOf e
          addSpanField child databaseErrorDetails $ show e
        )
        (\child -> liftIO $ do
          annotateBasics child conn
          connRollback conn preparer
        )

annotateBasics :: MonadIO m => MutableSpan -> SqlBackend -> m ()
annotateBasics s conn = addSpanFields s $ H.fromList
  [ (packageField, String "persistent/esqueleto")
  , (serviceNameField, String $ connRDBMS conn)
  , (typeField, String "db")
  ]
