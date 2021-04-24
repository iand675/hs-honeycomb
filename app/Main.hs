{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad.Logger ( runStdoutLoggingT )
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT) )
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql ( rawSql, Single, SqlBackend )
import Honeycomb.Client ( initializeHoneycomb )
import Honeycomb.Tracing
    ( noOpSpanErrorHandler,
      HasServiceName(..),
      HasSpan(..),
      HasSpanErrorHandler(..),
      HasTracer(..),
      Span(EmptySpan),
      SpanErrorHandler,
      Tracer(Tracer) )
import Honeycomb.Tracing.Monad ( spanning )
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
import Yesod.Persist ( YesodPersist(..) )
import Database.Persist.Postgresql
    ( withPostgresqlConn, rawSql, Single, SqlBackend )
import Lens.Micro (lens)
import System.Environment ( getEnv )

-- | This is my data type. There are many like it, but this one is mine.
data Minimal = Minimal
  { minimalTracer :: Tracer
  , minimalCurrentSpan :: Span
  , minimalSqlConn :: SqlBackend
  , minimalSpanErrorHandler :: SpanErrorHandler
  }

instance HasTracer Minimal where
  tracerL = lens minimalTracer (\m t -> m { minimalTracer = t })

instance HasServiceName Minimal where
  serviceNameL = tracerL . serviceNameL

instance HasSpan Minimal where
  spanL = lens minimalCurrentSpan (\m s -> m { minimalCurrentSpan = s })

instance HasSpanErrorHandler Minimal where
  spanErrorHandlerL = lens minimalSpanErrorHandler (\m e -> m { minimalSpanErrorHandler = e })

mkYesod "Minimal" [parseRoutes|
    / RootR GET
|]

instance Yesod Minimal where
  yesodMiddleware = Yesod.beelineMiddleware . defaultYesodMiddleware

instance YesodPersist Minimal where
  type YesodPersistBackend Minimal = SqlBackend
  runDB m = do
    app <- getYesod
    conn' <- wrapConnection (minimalCurrentSpan app) (minimalSqlConn app)
    runReaderT m conn'


getRootR :: Handler Text
getRootR = do
  (result :: [Single Text]) <- runDB $ rawSql "select usename from pg_catalog.pg_user" []

  spanning "getGoogle" $ do
    httpNoBody "https://google.com"

  pure ("Hello, " <> T.pack (show result))

main :: IO ()
main = do
  writeKey <- getEnv "HONEYCOMB_TEAM_WRITE_KEY"
  c <- initializeHoneycomb $ config (T.pack writeKey) (DatasetName "testing-client")
  let traceConf = Tracer c "testing-wai" [] Always UUIDGenerator
  putStrLn "Running"
  runStdoutLoggingT $ withPostgresqlConn "host=localhost port=5432 user=postgres" $ \conn -> do
    app <- liftIO $ toWaiApp $ Minimal traceConf EmptySpan conn noOpSpanErrorHandler
    liftIO $ runEnv 3000 $ Wai.beelineMiddleware traceConf app