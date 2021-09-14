{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
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
    ( httpNoBody, httpJSON, getResponseBody )
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
      Yesod(yesodMiddleware), Route )
import Yesod.Core.Handler
import Yesod.Persist ( YesodPersist(..) )
import Database.Persist.Postgresql
    ( withPostgresqlConn, rawSql, Single, SqlBackend, createPostgresqlPool )
import Control.Lens (lens, (^.))
import System.Environment ( getEnv )
import Chronos (now)
import Honeycomb.API.Markers
import Honeycomb.Tracing (HasTraceContext(..), TraceContext(..))
import Data.Pool (Pool)
import Honeycomb.Tracing.Instrumentation.Persistent (runSqlPool)
import Deriving.Aeson
import GHC.Generics
import Network.HTTP.Types (Header)
import Network.Wai (requestHeaders)
import Honeycomb.Tracing.Propagation
import Honeycomb.Tracing.Propagation.V1PropagationCodec

-- | This is my data type. There are many like it, but this one is mine.
data Minimal = Minimal
  { minimalTraceContext :: TraceContext
  , minimalSqlConn :: Pool SqlBackend
  , minimalHttpCodecs :: [PropagationCodec [Header]]
  }

instance HasTraceContext Minimal where
  traceContextL = lens minimalTraceContext (\m t -> m { minimalTraceContext = t })

instance HasCodecs Minimal [Header] where
  codecsL = lens minimalHttpCodecs (\m c -> m { minimalHttpCodecs = c })

mkYesod "Minimal" [parseRoutes|
/ RootR GET
/admin AdminR:
  /woo/#{userId::Int} AdminIndexR GET
/ping PingR GET
/pong PongR GET
|]

deriving instance Generic AdminR
deriving instance Generic (Route Minimal)
-- TODO we could do better json instances
deriving via CustomJSON '[SumUntaggedValue] AdminR instance ToJSON AdminR
deriving via CustomJSON '[SumUntaggedValue] (Route Minimal) instance ToJSON (Route Minimal)

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


getAdminR :: Handler Text
getAdminR = pure "ADMIN"

getAdminIndexR :: Int -> Handler Text
getAdminIndexR x = pure ("ADMIN_WOO" <> T.pack (show x))

getPingR :: Handler Value
getPingR = do
  getResponseBody <$> httpJSON "http://localhost:3000/pong"

getPongR :: Handler Value
getPongR = do
  req <- waiRequest
  liftIO $ print $ requestHeaders req
  pure $ object ["neat" .= True]

main :: IO ()
main = do
  writeKey <- getEnv "HONEYCOMB_TEAM_WRITE_KEY"
  c <- initializeHoneycomb $ config (T.pack writeKey) (DatasetName "testing-client")
  let traceConf = Tracer c "testing-wai" Always UUIDGenerator Nothing
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
    let minimal = Minimal (TraceContext (traceConf ^. serviceNameL) traceConf EmptySpan []) conn [v1PropagationCodec]
    app <- liftIO $ toWaiApp minimal
    liftIO $ runEnv 3000 $ Wai.beelineMiddleware traceConf (minimalHttpCodecs minimal) app