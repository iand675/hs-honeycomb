module Honeycomb.Tracing.Instrumentation.Conduit where

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Text (Text)
import Honeycomb.Tracing.Monad

spanningC :: 
     (MonadTrace env m, MonadResource m)
  => Text -- ^ Name of the span in question
  -> ConduitT i o m r 
  -> ConduitT i o m r
spanningC name_ c = undefined -- bracketP (Raw.newSpan _ _ _ _) Raw.closeSpan $ \span_
  -- catchC c $ \