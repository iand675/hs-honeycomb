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
import Data.RingBuffer
import Honeycomb.Client.Internal
import qualified Honeycomb.API.Events as API
import qualified Honeycomb.API.Types as API
import Network.HTTP.Client.TLS
import UnliftIO.Async
import UnliftIO
import Control.Monad.Reader
import Lens.Micro.Mtl (view)
import Lens.Micro ((%~))

newtype Worker = Worker
  { worker :: Async ()
  }

initializeHoneycomb :: MonadIO m => Config -> m HoneycombClient
initializeHoneycomb conf = liftIO $ do
  rand <- liftIO createSystemRandom
  buf <- liftIO $ new 32768 -- TODO allow configuration
  dummyWorker <- async $ pure ()
  let client = HoneycombClient conf rand buf dummyWorker
  innerWorker <- async $ forever $ do
    vec <- takeBatchBlocking buf
    -- TODO handle dispatch to multiple places, etc.
    -- TODO handle sample rate
    -- TODO log errors or something
    resp <- flip runReaderT client $ mapM API.sendEvent
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

send :: (MonadHoneycomb env m) => Event -> m ()
send e = do
  c@HoneycombClient{..} <- view honeycombClientL
  let specifiedSampleRate = _sampleRate e <|> sampleRate clientConfig
  (shouldSend, _sampleVal) <- case specifiedSampleRate of
    Nothing -> pure (True, 0)
    Just 1 -> pure (True, 0)
    Just n -> liftIO $ do
      x <- uniformR (1, n) clientGen
      pure (1 == x, x)
  when shouldSend $ do
    let event_ = API.Event specifiedSampleRate (_timestamp e) (_fields e)
    let localOptions = honeycombClientL %~ (\c -> c { clientConfig = replaceDataset $ replaceHost $ replaceWriteKey clientConfig })
    let blockingEvent = local localOptions $ API.sendEvent event_
    if sendBlocking clientConfig
    then blockingEvent
    -- TODO this is a wrong implementation
    else push e clientEventBuffer
  where
    replaceDataset c' = maybe c' (\ds -> c' { defaultDataset = ds }) $ _dataset e
    replaceHost c' = maybe c' (\h -> c' { apiHost = h }) $ _apiHost e
    replaceWriteKey c' = maybe c' (\k -> c' { teamWritekey = k }) $ _teamWriteKey e