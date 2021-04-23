{-# LANGUAGE RecordWildCards #-}
module Honeycomb.Client where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.HashMap.Strict as S
import Data.Maybe
import System.Random.MWC
import Honeycomb.Types
import Data.RingBuffer
import Honeycomb.Client.Internal
import qualified Honeycomb.API.Events as API
import qualified Honeycomb.API.Types as API
import Network.HTTP.Client.TLS
import UnliftIO.Async
import UnliftIO

newtype Worker = Worker
  { worker :: Async ()
  }

initializeHoneycomb :: MonadUnliftIO m => Config -> m HoneycombClient
initializeHoneycomb conf = do
  rand <- liftIO createSystemRandom
  m <- liftIO getGlobalManager
  buf <- liftIO $ new 32768 -- TODO allow configuration
  dummyWorker <- async $ pure ()
  let client = HoneycombClient m conf rand buf dummyWorker
  innerWorker <- async $ forever $ do
    vec <- takeBatchBlocking buf
    -- TODO handle dispatch to multiple places, etc.
    -- TODO handle sample rate
    -- TODO log errors or something
    resp <- mapM (API.sendEvent client (defaultDataset conf))
      $ fmap (\Event{..} -> API.Event Nothing _timestamp _fields) vec
    pure ()
  pure $ client { clientWorker = innerWorker }
  


shutdownHoneycomb :: MonadIO m => HoneycombClient -> m ()
shutdownHoneycomb = cancel . clientWorker

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

send :: MonadIO m => HoneycombClient -> Event -> m ()
send c@HoneycombClient{..} e = do
  let specifiedSampleRate = _sampleRate e <|> sampleRate clientConfig
  (shouldSend, _sampleVal) <- case specifiedSampleRate of
    Nothing -> pure (True, 0)
    Just 1 -> pure (True, 0)
    Just n -> liftIO $ do
      x <- uniformR (1, n) clientGen
      pure (1 == x, x)
  
  when shouldSend $ do
    let event_ = API.Event specifiedSampleRate (_timestamp e) (_fields e)
    let blockingEvent = API.sendEvent
          (c { clientConfig = replaceHost $ replaceWriteKey clientConfig })
          (fromMaybe (defaultDataset clientConfig) $ _dataset e) 
          event_
    liftIO $ if sendBlocking clientConfig
      then blockingEvent
      -- TODO this is a wrong implementation
      else push e clientEventBuffer
  where
    replaceHost c' = maybe c' (\h -> c' { apiHost = h }) $ _apiHost e
    replaceWriteKey c' = maybe c' (\k -> c' { teamWritekey = k }) $ _teamWriteKey e