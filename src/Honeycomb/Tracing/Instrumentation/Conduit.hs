module Honeycomb.Tracing.Instrumentation.Conduit where

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Text (Text)
import Honeycomb.Tracing.Monad
import qualified Honeycomb.Tracing.Raw as Raw
import Honeycomb.Tracing
import Conduit (lift, MonadIO (liftIO))
import Control.Monad.Reader
import UnliftIO

spanningC :: 
     (MonadTrace env m, MonadResource m, MonadUnliftIO m)
  => Text -- ^ Name of the span in question
  -> ConduitT i o m r 
  -> ConduitT i o m r
spanningC name_ c = do
  svc <- askServiceName
  span_ <- askSpan
  env <- lift ask
  _errorHandler <- askErrorHandler
  bracketP 
    (runReaderT (Raw.newSpan (trace span_) svc (Just $ spanId span_) name_) env)
    Raw.closeSpan
    (\child -> case _errorHandler of 
      SpanErrorHandler errHandler -> catchC c (\e -> liftIO (errHandler child e) *> throwIO e))
