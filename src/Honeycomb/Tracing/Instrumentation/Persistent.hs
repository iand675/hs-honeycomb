{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Honeycomb.Tracing.Instrumentation.Persistent where

import Conduit
import Data.Acquire.Internal
import Data.Typeable
import Database.Persist.Sql.Types.Internal
import Honeycomb.Tracing
import Honeycomb.Tracing.Fields
import Control.Monad.Reader (MonadReader)
import UnliftIO
import Data.Text (Text)

wrapConnection :: (MonadUnliftIO m, MonadReader r m, HasTraceConfig r) => Span -> SqlBackend -> m SqlBackend
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
              (traceConfigServiceName $ traceConfig trace)
              (Just spanId)
              "execute"
              (\child (e :: SomeException) -> do
                addField child databaseError $ show $ typeOf e
                addField child databaseErrorDetails $ show e
              )
              (\child -> do
                addField child packageField ("persistent/esqueleto" :: Text)
                addField child databaseQueryField t
                addField child databaseQueryParametersField $ show ps
                liftIO $ stmtExecute stmt ps
              )

          , stmtQuery = \ps -> do
              -- TODO this is tricky
              child <- mkAcquire (unliftIO m $ newSpan
                    trace
                    (traceConfigServiceName $ traceConfig trace)
                    (Just spanId)
                    "query") closeSpan

              addField child packageField ("persistent/esqueleto" :: Text)
              addField child databaseQueryField t
              addField child databaseQueryParametersField $ show ps

              -- TODO how to catch error info
              case stmtQuery stmt ps of
                Acquire stmtQueryAcquireF -> Acquire $ \f -> handle (queryErrorHandler child) (stmtQueryAcquireF f)
          }
    }
  where
    queryErrorHandler child (e :: SomeException) = case e of
      SomeException e' -> do
        addField child databaseError $ show $ typeOf e'
        addField child databaseErrorDetails $ show e'
        throwIO e'

    wrappedBegin m preparer iso = unliftIO m $ do
      spanning
        trace
        (traceConfigServiceName $ traceConfig trace)
        (Just spanId)
        "connBegin"
        (\child (e :: SomeException) -> do
          addField child databaseError $ show $ typeOf e
          addField child databaseErrorDetails $ show e
        )
        (\_child -> do
          liftIO $ connBegin conn preparer iso
        )
    wrappedCommit m preparer = unliftIO m $ do
      spanning
        trace
        (traceConfigServiceName $ traceConfig trace)
        (Just spanId)
        "connCommit"
        (\child (e :: SomeException) -> do
          addField child databaseError $ show $ typeOf e
          addField child databaseErrorDetails $ show e
        )
        (\_child -> do
          liftIO $ connCommit conn preparer
        )
    wrappedRollback m preparer = unliftIO m $ do
      spanning
        trace
        (traceConfigServiceName $ traceConfig trace)
        (Just spanId)
        "connRollback"
        (\child (e :: SomeException) -> do
          addField child databaseError $ show $ typeOf e
          addField child databaseErrorDetails $ show e
        )
        (\_child -> do
          liftIO $ connRollback conn preparer
        )
