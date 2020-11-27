module Data.Queue where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data QueueIO a = QueueIO
  { intVarsQueueIO  :: !(VUM.IOVector Int)
  , internalQueueIO :: !(VUM.IOVector a)
  }

data QueueST s a = QueueST
  { intVarsQueueST  :: !(VUM.STVector s Int)
  , internalQueueST :: !(VUM.STVector s a)
  }

_dequeueCount :: Int
_dequeueCount = 0
{-# INLINE _dequeueCount #-}

_enqueueCount :: Int
_enqueueCount = 1
{-# INLINE _enqueueCount #-}

defaultQueueSize :: Int
defaultQueueSize = 1024 * 1024

newQueueIO  :: VUM.Unbox a => Int -> IO (QueueIO a)
newQueueIO n = QueueIO <$> VUM.replicate 2 0 <*> VUM.unsafeNew n

newQueueST  :: VUM.Unbox a => Int -> ST s (QueueST s a)
newQueueST n = QueueST <$> VUM.replicate 2 0 <*> VUM.unsafeNew n

lengthQueueIO :: VUM.Unbox a => QueueIO a -> IO Int
lengthQueueIO (QueueIO info _) = (-) <$> VUM.unsafeRead info _enqueueCount <*> VUM.unsafeRead info _dequeueCount
{-# INLINE lengthQueueIO #-}

lengthQueueST :: VUM.Unbox a => QueueST s a -> ST s Int
lengthQueueST (QueueST info _) = (-) <$> VUM.unsafeRead info _enqueueCount <*> VUM.unsafeRead info _dequeueCount
{-# INLINE lengthQueueST #-}

dequeueQueueIO :: VUM.Unbox a => QueueIO a -> IO (Maybe a)
dequeueQueueIO (QueueIO info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  if f < r
    then do
      VUM.unsafeWrite info _dequeueCount (f + 1)
      pure <$> VUM.unsafeRead q f
    else return Nothing
{-# INLINE dequeueQueueIO #-}

dequeueQueueST :: VUM.Unbox a => QueueST s a -> ST s (Maybe a)
dequeueQueueST (QueueST info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  if f < r
    then do
      VUM.unsafeWrite info _dequeueCount (f + 1)
      pure <$> VUM.unsafeRead q f
    else return Nothing
{-# INLINE dequeueQueueST #-}

enqueueQueueIO :: VUM.Unbox a => QueueIO a -> a -> IO ()
enqueueQueueIO (QueueIO info q) x = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite q r x
  VUM.unsafeWrite info _enqueueCount (r + 1)
{-# INLINE enqueueQueueIO #-}

enqueueQueueST :: VUM.Unbox a => QueueST s a -> a -> ST s ()
enqueueQueueST (QueueST info q) x = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite q r x
  VUM.unsafeWrite info _enqueueCount (r + 1)
{-# INLINE enqueueQueueST #-}

enqueuesQueueIO :: VUM.Unbox a => QueueIO a -> VU.Vector a -> IO ()
enqueuesQueueIO (QueueIO info q) vec = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite info _enqueueCount (r + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice r (VU.length vec) q) vec
{-# INLINE enqueuesQueueIO #-}

enqueuesQueueST :: VUM.Unbox a => QueueST s a -> VU.Vector a -> ST s ()
enqueuesQueueST (QueueST info q) vec = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite info _enqueueCount (r + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice r (VU.length vec) q) vec
{-# INLINE enqueuesQueueST #-}

clearQueueIO :: VUM.Unbox a => QueueIO a -> IO ()
clearQueueIO (QueueIO info _) = do
  VUM.unsafeWrite info _dequeueCount 0
  VUM.unsafeWrite info _enqueueCount 0

clearQueueST :: VUM.Unbox a => QueueST s a -> ST s ()
clearQueueST (QueueST info _) = do
  VUM.unsafeWrite info _dequeueCount 0
  VUM.unsafeWrite info _enqueueCount 0

freezeQueueIO :: VUM.Unbox a => QueueIO a -> IO (VU.Vector a)
freezeQueueIO (QueueIO info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  VU.unsafeFreeze $ VUM.unsafeSlice f (r - f) q

freezeQueueST :: VUM.Unbox a => QueueST s a -> ST s (VU.Vector a)
freezeQueueST (QueueST info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  VU.unsafeFreeze $ VUM.unsafeSlice f (r - f) q
