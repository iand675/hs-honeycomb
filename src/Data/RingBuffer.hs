-- | This is a thread-safe implementation of a mutable ring-buffer
-- built upon @vector@.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.RingBuffer 
  ( RingBuffer
  , WriteMode(..)
  , new
  , push
  , tryPush
  , peekBatch
  , takeBatchBlocking
  , takeBatchNonBlocking
  , size
  , Data.RingBuffer.empty
  , full
  ) where
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Control.Applicative
import Control.Monad.State
import Control.Monad.Primitive
import Prelude hiding (length, concat)
import UnliftIO
import Data.Bits ((.&.))
import Control.Monad.STM (retry)
import Data.Functor

data WriteMode = SingleWriter | MultiWriter

type family WriterVar (mode :: WriteMode) :: * where
    WriterVar 'SingleWriter = TVar Int
    WriterVar 'MultiWriter = TMVar Int

class WriteOps a where
    initializeWriterVar :: Int -> IO a
    peekWriterVarValue :: a -> STM Int
    takeWriterVarValue :: a -> STM Int
    putWriterVarValue :: a -> Int -> STM ()

instance WriteOps (TVar Int) where
    initializeWriterVar = newTVarIO
    peekWriterVarValue = readTVar
    takeWriterVarValue = readTVar
    putWriterVarValue = writeTVar

instance WriteOps (TMVar Int) where
    initializeWriterVar = newTMVarIO
    peekWriterVarValue = readTMVar
    takeWriterVarValue = takeTMVar
    putWriterVarValue = putTMVar


-- | A concurrent ring buffer. Not lock-free. Size must be a power of two.
data RingBuffer (writeMode :: WriteMode) v a = RingBuffer
    { ringBuffer :: (VG.Mutable v) (PrimState IO) a
    , ringWrite :: !(WriterVar writeMode)
    , ringRead :: !(TVar Int)
    }

maskInt :: (VGM.MVector (VG.Mutable v) a) => RingBuffer writeMode v a -> Int -> Int
maskInt RingBuffer{..} x = x .&. (VGM.length ringBuffer - 1)

-- | Create a new ring of a given length
--
-- /Note:/ size must be a power of 2
new :: (MonadIO m, MonadFail m, WriteOps (WriterVar writeMode)) => (VG.Vector v a) => Int -> m (RingBuffer writeMode v a)
new n = do
    when (n < 1) $ fail "Data.RingBuffer.new: must be a positive power of two"
    buffer <- liftIO $ VGM.new n
    writeT <- liftIO $ initializeWriterVar 0
    readT <- newTVarIO 0
    pure $ RingBuffer
      { ringBuffer = buffer
      , ringWrite = writeT
      , ringRead = readT
      }

size :: (WriteOps (WriterVar writeMode)) => RingBuffer writeMode v a -> STM Int
size RingBuffer{..} = (-) <$> peekWriterVarValue ringWrite <*> readTVar ringRead

empty :: (VGM.MVector (VG.Mutable v) a, (WriteOps (WriterVar writeMode))) => RingBuffer writeMode v a -> STM Bool
empty RingBuffer{..} = (==) <$> peekWriterVarValue ringWrite <*> readTVar ringRead

full :: (VGM.MVector (VG.Mutable v) a, (WriteOps (WriterVar writeMode))) => RingBuffer writeMode v a -> STM Bool
full buf = (VGM.length (ringBuffer buf) ==) <$> size buf

-- | Add an item to the end of the ring
push :: MonadIO m => (VG.Vector v a, (WriteOps (WriterVar writeMode))) => a -> RingBuffer writeMode v a -> m ()
push x rb@RingBuffer{..} = liftIO $ mask_ $ do
    writeCounter <- atomically $ do
        isFull <- full rb
        when isFull retry
        takeWriterVarValue ringWrite
    liftIO $ VGM.unsafeWrite ringBuffer (maskInt rb writeCounter) x
    atomically $ putWriterVarValue ringWrite (writeCounter + 1)
    pure ()

-- | Add an item to the end of the ring
tryPush :: MonadIO m => (VG.Vector v a, (WriteOps (WriterVar writeMode))) => a -> RingBuffer writeMode v a -> m Bool
tryPush x rb@RingBuffer{..} = liftIO $ mask_ $ do
    join $ atomically $ do
        isFull <- full rb
        writeCounter <- takeWriterVarValue ringWrite
        pure $ if isFull
            then pure False
            else liftIO $ do
                VGM.unsafeWrite ringBuffer (maskInt rb writeCounter) x
                atomically $ putWriterVarValue ringWrite (writeCounter + 1)
                pure True



-- | Read all unread items
peekBatch :: (MonadIO m, VG.Vector v a, WriteOps (WriterVar writeMode)) => RingBuffer writeMode v a -> m (v a)
peekBatch rb@RingBuffer{..} = do
    (writeCounter, readCounter) <- atomically $ do
        writeCounter <- peekWriterVarValue ringWrite
        readCounter <- readTVar ringRead
        pure (writeCounter, readCounter)
    let maskedWrite = maskInt rb writeCounter
    let maskedRead = maskInt rb readCounter
    case writeCounter `compare` readCounter of
        --   0 1 2 3 4 5 6 7 8
        --       ^write
        --       ^read
        EQ -> pure VG.empty
        --   0 1 2 3 4 5 6 7 8
        --         ^ write
        --   ^read
        GT -> liftIO $ do
            -- ob <- VG.freeze ringBuffer
            -- print (maskedRead, writeCounter - readCounter, ob)
            VG.freeze $ VGM.slice maskedRead (writeCounter - readCounter) ringBuffer
        --   0 1 2 3 4 5 6 7 8
        --         ^ read
        --       ^ write
        LT -> liftIO $ do
            copyVec <- VGM.new (maskedRead - maskedWrite)
            let lowerSliceOld = VGM.take maskedWrite ringBuffer
                (lowerSliceNew, upperSliceNew) = VGM.splitAt maskedWrite copyVec
                upperSliceOld = VGM.drop maskedRead ringBuffer
            VGM.copy lowerSliceOld lowerSliceNew
            VGM.copy upperSliceOld upperSliceNew
            VG.unsafeFreeze copyVec

-- TODO, we would have to take and then put to preserve atomicity guarantees
takeBatchBlocking :: (MonadIO m, VG.Vector v a, WriteOps (WriterVar writeMode)) => RingBuffer writeMode v a -> m (v a)
takeBatchBlocking rb@RingBuffer{..} = do
    (writeCounter, readCounter) <- atomically $ do
        isEmpty <- Data.RingBuffer.empty rb
        when isEmpty retry
        writeCounter <- peekWriterVarValue ringWrite
        readCounter <- readTVar ringRead
        pure (writeCounter, readCounter)
    let maskedWrite = maskInt rb writeCounter
    let maskedRead = maskInt rb readCounter
    result <- case writeCounter `compare` readCounter of
        --   0 1 2 3 4 5 6 7 8
        --       ^write
        --       ^read
        EQ -> pure VG.empty
        --   0 1 2 3 4 5 6 7 8
        --         ^ write
        --   ^read
        GT -> liftIO $ do
            -- ob <- VG.freeze ringBuffer
            -- print (maskedRead, writeCounter - readCounter, ob)
            VG.freeze $ VGM.slice maskedRead (writeCounter - readCounter) ringBuffer
        --   0 1 2 3 4 5 6 7 8
        --         ^ read
        --       ^ write
        LT -> liftIO $ do
            copyVec <- VGM.new (maskedRead - maskedWrite)
            let lowerSliceOld = VGM.take maskedWrite ringBuffer
                (lowerSliceNew, upperSliceNew) = VGM.splitAt maskedWrite copyVec
                upperSliceOld = VGM.drop maskedRead ringBuffer
            VGM.copy lowerSliceOld lowerSliceNew
            VGM.copy upperSliceOld upperSliceNew
            VG.unsafeFreeze copyVec
    atomically (writeTVar ringRead writeCounter $> result)

-- TODO, we would have to take and then put to preserve atomicity guarantees
takeBatchNonBlocking :: (MonadIO m, VG.Vector v a, WriteOps (WriterVar writeMode)) => RingBuffer writeMode v a -> m (v a)
takeBatchNonBlocking rb@RingBuffer{..} = do
    (writeCounter, readCounter) <- atomically $ do
        writeCounter <- peekWriterVarValue ringWrite
        readCounter <- readTVar ringRead
        pure (writeCounter, readCounter)
    let maskedWrite = maskInt rb writeCounter
    let maskedRead = maskInt rb readCounter
    result <- case writeCounter `compare` readCounter of
        --   0 1 2 3 4 5 6 7 8
        --       ^write
        --       ^read
        EQ -> pure VG.empty
        --   0 1 2 3 4 5 6 7 8
        --         ^ write
        --   ^read
        GT -> liftIO $ do
            -- ob <- VG.freeze ringBuffer
            -- print (maskedRead, writeCounter - readCounter, ob)
            VG.freeze $ VGM.slice maskedRead (writeCounter - readCounter) ringBuffer
        --   0 1 2 3 4 5 6 7 8
        --         ^ read
        --       ^ write
        LT -> liftIO $ do
            copyVec <- VGM.new (maskedRead - maskedWrite)
            let lowerSliceOld = VGM.take maskedWrite ringBuffer
                (lowerSliceNew, upperSliceNew) = VGM.splitAt maskedWrite copyVec
                upperSliceOld = VGM.drop maskedRead ringBuffer
            VGM.copy lowerSliceOld lowerSliceNew
            VGM.copy upperSliceOld upperSliceNew
            VG.unsafeFreeze copyVec
    atomically (writeTVar ringRead writeCounter $> result)

{-
test :: IO ()
test = do
    rb <- new 8 :: IO (RingBuffer 'SingleWriter V.Vector Int)
    writer <- async $ do
        counter <- newIORef 0
        forever $ do
            x <- atomicModifyIORef' counter (\x -> (x + 1, x + 1))
            tryPush x rb >>= print
            yield
    reader <- async $ forever $ do
        takeBatchNonBlocking rb >>= print
        -- b <- takeBatchNonBlocking rb
        -- print b
    _ <- getLine
    cancel writer
    cancel reader
-}