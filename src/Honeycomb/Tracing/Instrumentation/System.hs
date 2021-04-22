{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Honeycomb.Tracing.Instrumentation.System where
import Control.AutoUpdate
import qualified Data.HashMap.Strict as M
import qualified GHC.Stats as Stats
import Data.Int
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import Data.Aeson (Value, ToJSON (toJSON))
import System.Posix
import Control.Monad.IO.Class (MonadIO (liftIO))
import Honeycomb.Tracing

annotateTraceWithSystemInfo :: MonadIO m => Trace -> m ()
annotateTraceWithSystemInfo t = do
  metrics <- readRtsMetrics
  let (CPid pid) = currentProcessId
  addTraceField t "proc.id" pid
  addTraceFields t metrics

currentProcessId :: ProcessID
currentProcessId = unsafePerformIO getProcessID
{-# NOINLINE currentProcessId #-}

debouncedRtsStats :: IO Stats.RTSStats
debouncedRtsStats = unsafePerformIO $ mkAutoUpdate (defaultUpdateSettings { updateAction = Stats.getRTSStats })
{-# NOINLINE debouncedRtsStats #-}
------------------------------------------------------------------------
-- * Predefined metrics

-- $predefined
-- This library provides a number of pre-defined metrics that can
-- easily be added to a metrics store by calling their register
-- function.
#if MIN_VERSION_base(4,10,0)
-- | Convert nanoseconds to milliseconds.
nsToMs :: Int64 -> Int64
nsToMs s = round (realToFrac s / (1000000.0 :: Double))
#else
-- | Convert seconds to milliseconds.
sToMs :: Double -> Int64
sToMs s = round (s * 1000.0)
#endif

-- [@rts.gc.par_tot_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
--
-- [@rts.gc.par_avg_bytes_copied@] Deprecated alias for
-- @par_tot_bytes_copied@.
--
-- [@rts.gc.par_max_bytes_copied@] Sum of number of bytes copied each GC by
-- the most active GC thread each GC. The ratio of
-- @par_tot_bytes_copied@ divided by @par_max_bytes_copied@ approaches
-- 1 for a maximally sequential run and approaches the number of
-- threads (set by the RTS flag @-N@) for a maximally parallel run.

-- TODO need to figure out how to do counters vs gauges in this case
readRtsMetrics :: MonadIO m => m (M.HashMap Text Value)
readRtsMetrics = liftIO $ do
#if MIN_VERSION_base(4,10,0)
  stats <- Stats.getRTSStats
  pure $ M.fromList
    [ ("rts.gc.bytes_allocated"          , toJSON $ Stats.allocated_bytes stats)
    , ("rts.gc.num_gcs"                  , toJSON $ Stats.gcs stats)
    , ("rts.gc.num_bytes_usage_samples"  , toJSON $ Stats.major_gcs stats)
    , ("rts.gc.cumulative_bytes_used"    , toJSON $ Stats.cumulative_live_bytes stats)
    , ("rts.gc.bytes_copied"             , toJSON $ Stats.copied_bytes stats)
#if MIN_VERSION_base(4,12,0)
    , ("rts.gc.init_cpu_ms"              , toJSON $ nsToMs $ Stats.init_cpu_ns stats)
    , ("rts.gc.init_wall_ms"             , toJSON $ nsToMs $ Stats.init_elapsed_ns stats)
#endif
    , ("rts.gc.mutator_cpu_ms"           , toJSON $ nsToMs $ Stats.mutator_cpu_ns stats)
    , ("rts.gc.mutator_wall_ms"          , toJSON $ nsToMs $ Stats.mutator_elapsed_ns stats)
    , ("rts.gc.gc_cpu_ms"                , toJSON $ nsToMs $ Stats.gc_cpu_ns stats)
    , ("rts.gc.gc_wall_ms"               , toJSON $ nsToMs $ Stats.gc_elapsed_ns stats)
    , ("rts.gc.cpu_ms"                   , toJSON $ nsToMs $ Stats.cpu_ns stats)
    , ("rts.gc.wall_ms"                  , toJSON $ nsToMs $ Stats.elapsed_ns stats)
    , ("rts.gc.max_bytes_used"           , toJSON $ Stats.max_live_bytes stats)
    , ("rts.gc.current_bytes_used"       , toJSON $ Stats.gcdetails_live_bytes $ Stats.gc stats)
    , ("rts.gc.current_bytes_slop"       , toJSON $ Stats.gcdetails_slop_bytes $ Stats.gc stats)
    , ("rts.gc.max_bytes_slop"           , toJSON $ Stats.max_slop_bytes stats)
    , ("rts.gc.peak_megabytes_allocated" , toJSON $ (`quot` (1024*1024)) $ Stats.max_mem_in_use_bytes stats)
    , ("rts.gc.par_tot_bytes_copied"     , toJSON $ Stats.par_copied_bytes stats)
    , ("rts.gc.par_avg_bytes_copied"     , toJSON $ Stats.par_copied_bytes stats)
    , ("rts.gc.par_max_bytes_copied"     , toJSON $ Stats.cumulative_par_max_copied_bytes stats)
    ]
#else
  stats <- Stats.getGcStats
  pure $ M.fromList
    [ ("rts.gc.bytes_allocated"          , toJSON $ Stats.bytesAllocated stats)
    , ("rts.gc.num_gcs"                  , toJSON $ Stats.numGcs stats)
    , ("rts.gc.num_bytes_usage_samples"  , toJSON $ Stats.numByteUsageSamples stats)
    , ("rts.gc.cumulative_bytes_used"    , toJSON $ Stats.cumulativeBytesUsed stats)
    , ("rts.gc.bytes_copied"             , toJSON $ Stats.bytesCopied stats)
    , ("rts.gc.mutator_cpu_ms"           , toJSON $ sToMs $ Stats.mutatorCpuSeconds stats)
    , ("rts.gc.mutator_wall_ms"          , toJSON $ sToMs $ Stats.mutatorWallSeconds stats)
    , ("rts.gc.gc_cpu_ms"                , toJSON $ sToMs $ Stats.gcCpuSeconds stats)
    , ("rts.gc.gc_wall_ms"               , toJSON $ sToMs $ Stats.gcWallSeconds stats)
    , ("rts.gc.cpu_ms"                   , toJSON $ sToMs $ Stats.cpuSeconds stats)
    , ("rts.gc.wall_ms"                  , toJSON $ sToMs $ Stats.wallSeconds stats)
    , ("rts.gc.max_bytes_used"           , toJSON $ Stats.maxBytesUsed stats)
    , ("rts.gc.current_bytes_used"       , toJSON $ Stats.currentBytesUsed stats)
    , ("rts.gc.current_bytes_slop"       , toJSON $ Stats.currentBytesSlop stats)
    , ("rts.gc.max_bytes_slop"           , toJSON $ Stats.maxBytesSlop stats)
    , ("rts.gc.peak_megabytes_allocated" , toJSON $ Stats.peakMegabytesAllocated stats)
    , ("rts.gc.par_tot_bytes_copied"     , toJSON $ gcParTotBytesCopied stats)
    , ("rts.gc.par_avg_bytes_copied"     , toJSON $ gcParTotBytesCopied stats)
    , ("rts.gc.par_max_bytes_copied"     , toJSON $ Stats.parMaxBytesCopied stats)
    ]
#endif