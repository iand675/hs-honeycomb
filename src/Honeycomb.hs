{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Honeycomb
Description : A simple interface to send events to Honeycomb.
Copyright   : (c) Ian Duncan, 2021
License     : BSD-3
Maintainer  : ian@iankduncan.com
Stability   : unstable
Portability : Portable

Warning, not all configuration options actually do what they claim yet.
-}
module Honeycomb
  (
  -- * Initializing and shutting down a 'HoneycombClient'
    HoneycombClient
  , initializeHoneycomb
  , Config.config
  , shutdownHoneycomb
  -- * Sending events
  , event
  , Event(..)
  , send
  -- * Embedding a HoneycombClient into larger applications
  , MonadHoneycomb
  , HasHoneycombClient(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict as S
import Data.Maybe
import System.Random.MWC
import qualified Honeycomb.Config as Config
import Honeycomb.Types
import Honeycomb.Client.Internal
import qualified Honeycomb.API.Events as API
import qualified Honeycomb.API.Types as API
import Network.HTTP.Client.TLS
import UnliftIO.Async hiding (atomically)
import UnliftIO
import Control.Monad.Reader
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TBQueue hiding (newTBQueueIO)
import Control.Concurrent
import Lens.Micro ((%~), (^.), (&))
import Lens.Micro.Extras (view)
import qualified Data.Aeson.KeyMap as KeyMap

initializeHoneycomb :: MonadIO m => Config.Config -> m HoneycombClient
initializeHoneycomb conf = liftIO $ do
  putStrLn "Initialize honeycomb client"
  rand <- liftIO createSystemRandom
  buf <- liftIO $ newTBQueueIO (fromIntegral $ Config.pendingQueueSize conf)
  sendThreadCount <- fmap (max 1) $ if Config.sendThreads conf == 0
    then liftIO (fmap fromIntegral getNumCapabilities)
    else pure $ fromIntegral $ Config.sendThreads conf
  liftIO $ print ("sendThreadCount"::String, sendThreadCount)
  -- TODO this will lose some events upon cancellation, so we need to handle that by properly
  -- flushing everything.
  innerWorkers <- liftIO $ replicateM (fromIntegral $ Config.sendThreads conf) $ async $ do
    putStrLn "Booting worker thread"
    forever $ do
      actions <- mask_ $ liftIO $ do
        items <- atomically $ flushTBQueue buf
        -- TODO do something better than exception printing
        handle (\e -> print (e :: SomeException) *> throwIO e) $ sequence_ items
        pure items
      -- A little hack to retry outside of mask so that way we can cancel in between outbound calls
      case actions of
        [] -> atomically $ void $ peekTBQueue buf
        _ -> pure ()
  pure $ HoneycombClient conf rand buf innerWorkers

shutdownHoneycomb :: MonadIO m => HoneycombClient -> m ()
shutdownHoneycomb = mapM_ cancel . clientWorkers

event :: Event
event = Event
  { fields = S.empty
  , teamWriteKey = Nothing
  , dataset = Nothing
  , apiHost = Nothing
  , sampleRate = Nothing
  , timestamp = Nothing
  }

class ToEventField a where
class ToEventFields a where

send :: (MonadIO m, HasHoneycombClient env) => env -> Event -> m ()
send hasC e = do
  let c@HoneycombClient{..} = hasC ^. honeycombClientL
      specifiedSampleRate = sampleRate e <|> Config.sampleRate clientConfig
  (shouldSend, _sampleVal) <- case specifiedSampleRate of
    Nothing -> pure (True, 0)
    Just 1 -> pure (True, 0)
    Just n -> liftIO $ do
      x <- uniformR (1, n) clientGen
      pure (1 == x, x)
  when shouldSend $ do
    let event_ = API.Event specifiedSampleRate (timestamp e) (KeyMap.fromHashMapText $ fields e)
        localOptions = honeycombClientL %~ (\c -> c { clientConfig = replaceDataset $ replaceHost $ replaceWriteKey clientConfig })
        blockingEvent = runReaderT (API.sendEvent event_) (c & localOptions)
    liftIO $ if Config.sendBlocking clientConfig
      then blockingEvent
      else atomically $ writeTBQueue clientEventBuffer blockingEvent
  where
    replaceDataset :: Config.Config -> Config.Config
    replaceDataset c' = maybe c' (\ds -> c' { Config.defaultDataset = ds }) $ dataset e
    replaceHost :: Config.Config -> Config.Config
    replaceHost c' = maybe c' (\h -> c' { Config.apiHost = h }) $ apiHost e
    replaceWriteKey :: Config.Config -> Config.Config
    replaceWriteKey c' = maybe c' (\k -> c' { Config.teamWritekey = k }) $ teamWriteKey e
