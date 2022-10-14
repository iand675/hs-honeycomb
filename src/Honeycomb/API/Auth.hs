module Honeycomb.API.Auth where

import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (eitherDecode)
import qualified Data.Text as T
import Honeycomb
  ( HasHoneycombClient (honeycombClientL),
    MonadHoneycomb,
  )
import Honeycomb.API.Auth.Types
  ( Auth,
    FailureResponse (FailureCode, JsonDecodeFailed),
  )
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
      Left r -> throw . JsonDecodeFailed . T.pack $ r
    other -> throw $ FailureCode other (getResponseBody r)
