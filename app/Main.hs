{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad
import Control.Monad.Logger ( runStdoutLoggingT )
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT) )
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql ( rawSql, Single, SqlBackend )
import qualified Data.HashMap.Strict as H
import Honeycomb.Client ( initializeHoneycomb )
import Honeycomb.Tracing
    ( HasServiceName(..),
      HasSpan(..),
      HasSpanErrorAnnotators(..),
      HasTracer(..),
      HasTrace(..),
      MutableSpan,
      Span(EmptySpan),
      SpanErrorAnnotator,
      Tracer(Tracer) )
import Honeycomb.Tracing.Monad ( asService, spanning, addEvent, addTraceField )
import Honeycomb.Types ( config, DatasetName(DatasetName) )
import Honeycomb.Tracing.Sampling ( Always(Always) )
import Honeycomb.Tracing.Ids.UUIDTraceIdProvider
    ( UUIDGenerator(UUIDGenerator) )
import Honeycomb.Tracing.Instrumentation.Wai as Wai
    ( beelineMiddleware )
import Honeycomb.Tracing.Instrumentation.Persistent as Persistent
    ( wrapConnection )
import Honeycomb.Tracing.Instrumentation.HttpClientSimple
    ( httpNoBody )
import Honeycomb.Tracing.Instrumentation.Yesod as Yesod
    ( beelineMiddleware )
import Network.Wai.Handler.Warp ( runEnv )
import Yesod.Core
    ( toWaiApp,
      getYesod,
      defaultYesodMiddleware,
      parseRoutes,
      mkYesod,
      MonadIO(liftIO),
      RenderRoute(renderRoute),
      Yesod(yesodMiddleware) )
import Yesod.Core.Handler
import Yesod.Persist ( YesodPersist(..) )
import Database.Persist.Postgresql
    ( withPostgresqlConn, rawSql, Single, SqlBackend, createPostgresqlPool )
import Lens.Micro (lens, (^.))
import System.Environment ( getEnv )
import Chronos (now)
import Honeycomb.API.Markers
import Honeycomb.Tracing (HasTraceContext(..), TraceContext(..))
import Data.Pool (Pool)
import Honeycomb.Tracing.Instrumentation.Persistent (runSqlPool)

-- | This is my data type. There are many like it, but this one is mine.
data Minimal = Minimal
  { minimalTraceContext :: TraceContext
  , minimalSqlConn :: Pool SqlBackend
  }

instance HasTraceContext Minimal where
  traceContextL = lens minimalTraceContext (\m t -> m { minimalTraceContext = t })

mkYesod "Minimal" [parseRoutes|
    / RootR GET
|]

instance Yesod Minimal where
  yesodMiddleware = Yesod.beelineMiddleware . defaultYesodMiddleware

instance YesodPersist Minimal where
  type YesodPersistBackend Minimal = SqlBackend
  runDB m = do
    app <- getYesod
    runSqlPool m (minimalSqlConn app)


getRootR :: Handler Text
getRootR = do
  mx <- lookupGetParam "user"
  case mx of
    Nothing -> pure ()
    Just "1" -> asService "googleFetcher" $ spanning "getGoogle" $ do
      addEvent "Oh hai" H.empty
      httpNoBody "https://google.com"
      addEvent "Oh bye" H.empty
      error "Oh no"
    Just "2" -> asService "googleFetcher" $ spanning "getGoogle" $ do
      addEvent "Oh hai" H.empty
      httpNoBody "https://google.com"
      addEvent "Oh bye" H.empty
    Just "3" -> replicateM_ 3 $ asService "googleFetcher" $ spanning "getGoogle" $ do
      addEvent "Oh hai" H.empty
      httpNoBody "https://google.com"
      addEvent "Oh bye" H.empty
    Just _ -> notFound
  (result :: [Single Text]) <- runDB $ do
    spanning "noodle" $ spanning "nesty nesty" $
      rawSql "select usename from pg_catalog.pg_user" []
  pure ("Hello, " <> T.pack (show result))


main :: IO ()
main = do
  writeKey <- getEnv "HONEYCOMB_TEAM_WRITE_KEY"
  c <- initializeHoneycomb $ config (T.pack writeKey) (DatasetName "testing-client")
  let traceConf = Tracer c "testing-wai" [] Always UUIDGenerator Nothing
  putStrLn "Running"
  t <- now
  let makeMarker = createMarker 
        (emptyMarker 
          { startTime = Just t
          , endTime = Just t
          , message = Just "Starting minimal app", markerType = Just "app.launch"}) 
  runReaderT makeMarker c
  runStdoutLoggingT $ do
    conn <- createPostgresqlPool "host=localhost port=5432 user=postgres" 5 
    app <- liftIO $ toWaiApp $ Minimal (TraceContext (traceConf ^. serviceNameL) traceConf EmptySpan []) conn 
    liftIO $ runEnv 3000 $ Wai.beelineMiddleware traceConf app