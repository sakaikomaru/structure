module Data.Queue where

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Queue a = Queue
  { intVarsQueue  :: !(VUM.IOVector Int)
  , internalQueue :: !(VUM.IOVector a)
  }

_dequeueCount :: Int
_dequeueCount = 0
{-# INLINE _dequeueCount #-}

_enqueueCount :: Int
_enqueueCount = 1
{-# INLINE _enqueueCount #-}

newQueue  :: VUM.Unbox a => Int -> IO (Queue a)
newQueue n = Queue <$> VUM.replicate 2 0 <*> VUM.unsafeNew n

defaultQueueSize :: Int
defaultQueueSize = 1024 * 1024

lengthQueue :: VUM.Unbox a => Queue a -> IO Int
lengthQueue (Queue info _) = (-) <$> VUM.unsafeRead info _enqueueCount <*> VUM.unsafeRead info _dequeueCount
{-# INLINE lengthQueue #-}

dequeueQueue :: VUM.Unbox a => Queue a -> IO (Maybe a)
dequeueQueue (Queue info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  if f < r
    then do
      VUM.unsafeWrite info _dequeueCount (f + 1)
      pure <$> VUM.unsafeRead q f
    else return Nothing
{-# INLINE dequeueQueue #-}

enqueueQueue :: VUM.Unbox a => Queue a -> a -> IO ()
enqueueQueue (Queue info q) x = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite q r x
  VUM.unsafeWrite info _enqueueCount (r + 1)
{-# INLINE enqueueQueue #-}

enqueuesQueue :: VUM.Unbox a => Queue a -> VU.Vector a -> IO ()
enqueuesQueue (Queue info q) vec = do
  r <- VUM.unsafeRead info _enqueueCount
  VUM.unsafeWrite info _enqueueCount (r + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice r (VU.length vec) q) vec
{-# INLINE enqueuesQueue #-}

clearQueue :: VUM.Unbox a => Queue a -> IO ()
clearQueue (Queue info _) = do
  VUM.unsafeWrite info _dequeueCount 0
  VUM.unsafeWrite info _enqueueCount 0

freezeQueue :: VUM.Unbox a => Queue a -> IO (VU.Vector a)
freezeQueue (Queue info q) = do
  f <- VUM.unsafeRead info _dequeueCount
  r <- VUM.unsafeRead info _enqueueCount
  VU.unsafeFreeze $ VUM.unsafeSlice f (r - f) q