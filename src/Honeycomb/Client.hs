{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Client
  ( initializeHoneycomb
  , shutdownHoneycomb
  , event
  , Event(..)
  , send
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict as S
import Data.Maybe
import System.Random.MWC
import Honeycomb.Types
import Honeycomb.Client.Internal
import qualified Honeycomb.API.Events as API
import qualified Honeycomb.API.Types as API
import Network.HTTP.Client.TLS
import UnliftIO.Async hiding (atomically)
import UnliftIO
import Control.Monad.Reader
import Lens.Micro.Mtl (view)
import Lens.Micro ((%~), (^.), (&))
import Control.Concurrent.STM (retry)
import Control.Concurrent.STM.TBQueue hiding (newTBQueueIO)
import Control.Concurrent

initializeHoneycomb :: MonadIO m => Config -> m HoneycombClient
initializeHoneycomb conf = liftIO $ do
  putStrLn "Initialize honeycomb client"
  rand <- liftIO createSystemRandom
  buf <- liftIO $ newTBQueueIO (fromIntegral $ pendingQueueSize conf)
  sendThreadCount <- fmap (max 1) $ if sendThreads conf == 0
    then liftIO (fmap fromIntegral getNumCapabilities)
    else pure $ fromIntegral $ sendThreads conf
  liftIO $ print ("sendThreadCount"::String, sendThreadCount)
  -- TODO this will lose some events upon cancellation, so we need to handle that by properly
  -- flushing everything.
  innerWorkers <- liftIO $ replicateM (fromIntegral $ sendThreads conf) $ async $ do
    putStrLn "Booting worker thread"
    forever $ do
      actions <- mask_ $ liftIO $ do
        items <- atomically $ flushTBQueue buf
        -- TODO do something better than exception printing
        print $ length items
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
  { _fields = S.empty
  , _teamWriteKey = Nothing
  , _dataset = Nothing
  , _apiHost = Nothing
  , _sampleRate = Nothing
  , _timestamp = Nothing
  }

class ToEventField a where
class ToEventFields a where

send :: (MonadIO m, HasHoneycombClient env) => env -> Event -> m ()
send hasC e = do
  let c@HoneycombClient{..} = hasC ^. honeycombClientL
      specifiedSampleRate = _sampleRate e <|> sampleRate clientConfig
  (shouldSend, _sampleVal) <- case specifiedSampleRate of
    Nothing -> pure (True, 0)
    Just 1 -> pure (True, 0)
    Just n -> liftIO $ do
      x <- uniformR (1, n) clientGen
      pure (1 == x, x)
  when shouldSend $ do
    let event_ = API.Event specifiedSampleRate (_timestamp e) (_fields e)
        localOptions = honeycombClientL %~ (\c -> c { clientConfig = replaceDataset $ replaceHost $ replaceWriteKey clientConfig })
        blockingEvent = runReaderT (API.sendEvent event_) (c & localOptions)
    liftIO $ if sendBlocking clientConfig
      then blockingEvent
      else atomically $ writeTBQueue clientEventBuffer blockingEvent
  where
    replaceDataset c' = maybe c' (\ds -> c' { defaultDataset = ds }) $ _dataset e
    replaceHost c' = maybe c' (\h -> c' { apiHost = h }) $ _apiHost e
    replaceWriteKey c' = maybe c' (\k -> c' { teamWritekey = k }) $ _teamWriteKey e