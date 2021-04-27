module Honeycomb.Tracing.Instrumentation.Katip where

import Katip
import Honeycomb.Tracing
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Honeycomb.Tracing.Raw as Raw
import Honeycomb.Tracing.Monad
import Control.Monad.Reader
import qualified Data.HashMap.Strict as H
import Data.Aeson ((.=))
import Lens.Micro

{-
honeycombSpanScribe :: MonadTrace m => m Scribe
honeycombSpanScribe = do
  env <- ask
  pure $ Scribe
    { liPush = \li -> flip runReaderT env $ do
        -- TODO this could be more efficient
        let logStr' = L.toStrict $ L.toLazyText $ unLogStr $ _itemMessage li
            payload = H.fromList
              [ -- "log.app" .= _itemApp li
              "log.env" .= _itemEnv li
              , "log.severity" .= _itemSeverity li
              , "log.thread" .= _itemThread li
              , "log.host" .= _itemHost li
              , "log.process" .= show (_itemProcess li)
              -- _itemMessage is just "name" due to how Honeycomb renders this in the UI
              -- _itemTime is set specially
              , "log.loc" .= fmap show (_itemLoc li)
              ]

        Raw.addEvent (env ^. spanL . traceL) (env ^. serviceNameL) (env ^. spanL . to spanId) logStr' (payload <> toObject (_itemPayload  li))
    , scribeFinalizer = pure ()
    , scribePermitItem = const $ pure True
    }
-}