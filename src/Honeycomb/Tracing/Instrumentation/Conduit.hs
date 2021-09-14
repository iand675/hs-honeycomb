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

instance (MonadTrace m, MonadResource m, MonadUnliftIO m) => MonadTrace (ConduitT i o m) where
  askTraceContext = lift askTraceContext
  localTraceContext f = transPipe (localTraceContext f)
  -- TODO support error annotations.
  spanning n m = do
    TraceContext{..} <- askTraceContext
    bracketP 
      (Raw.newSpan tcTracer (trace tcSpan) tcSvc (Just $ spanId tcSpan) n)
      Raw.closeSpan
      (\child ->  transPipe (localSpan (const child)) m)
