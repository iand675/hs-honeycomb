module Honeycomb.API.Auth (module Honeycomb.API.Auth.Types, getAuth) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (asks)
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import Honeycomb
  ( HasHoneycombClient (honeycombClientL),
    MonadHoneycomb,
  )
import Honeycomb.API.Auth.Types
import Honeycomb.Client.Internal (MonadHoneycombConfig, get)
import Lens.Micro.Extras (view)
import Network.HTTP.Client (Response (responseBody))
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpLBS)

getAuth :: (MonadIO m, MonadHoneycombConfig client m) => m Auth
getAuth = do
  r <- get httpLBS ["1", "auth"] []
  case getResponseStatusCode r of
    200 -> case eitherDecode (responseBody r) of
      Right r -> pure r
      Left r -> liftIO . throwIO . JsonDecodeFailed . T.pack $ r
    other -> liftIO . throwIO $ FailureCode other (getResponseBody r)
