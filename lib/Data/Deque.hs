module Data.Deque where

import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Deque a = Deque
  { dequeVars :: !(VUM.IOVector Int)
  , getDeque  :: !(VUM.IOVector a)
  }

_dequeFrontPos :: Int
_dequeFrontPos = 0

_dequeBackPos :: Int
_dequeBackPos = 1

newDeque :: VU.Unbox a => Int -> IO (Deque a)
newDeque n = Deque <$> VUM.replicate 2 n <*> VUM.unsafeNew (2 * n)

defaultDequeSize :: Int
defaultDequeSize = 1024 * 1024

lengthDeque :: VU.Unbox a => Deque a -> IO Int
lengthDeque (Deque info _) = (-) <$> VUM.unsafeRead info _dequeBackPos <*> VUM.unsafeRead info _dequeFrontPos

popFront :: VU.Unbox a => Deque a -> IO (Maybe a)
popFront (Deque info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  if f < b
    then do
        VUM.unsafeWrite info _dequeFrontPos (f + 1)
        pure <$> VUM.unsafeRead v f
    else return Nothing
{-# INLINE popFront #-}

popBack :: VU.Unbox a => Deque a -> IO (Maybe a)
popBack (Deque info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  if f < b
    then do
        VUM.unsafeWrite info _dequeBackPos (b - 1)
        pure <$> VUM.unsafeRead v (b - 1)
    else return Nothing
{-# INLINE popBack #-}

pushFront :: VU.Unbox a => Deque a -> a -> IO ()
pushFront (Deque info v) x = do
  f <- VUM.unsafeRead info _dequeFrontPos
  VUM.unsafeWrite v (f - 1) x
  VUM.unsafeWrite info _dequeFrontPos (f - 1)
{-# INLINE pushFront #-}

pushBack :: VU.Unbox a => Deque a -> a -> IO ()
pushBack (Deque info v) x = do
  b <- VUM.unsafeRead info _dequeBackPos
  VUM.unsafeWrite v b x
  VUM.unsafeWrite info _dequeBackPos (b + 1)
{-# INLINE pushBack #-}

pushFronts :: VU.Unbox a => Deque a -> VU.Vector a -> IO ()
pushFronts (Deque info v) vec = do
  let n = VU.length vec
  f <- VUM.unsafeRead info _dequeFrontPos
  VUM.unsafeWrite info _dequeFrontPos (f - n)
  VU.unsafeCopy (VUM.unsafeSlice (f - n) n v) vec
{-# INLINE pushFronts #-}

pushBacks :: VU.Unbox a => Deque a -> VU.Vector a -> IO ()
pushBacks (Deque info v) vec = do
  let n = VU.length vec
  b <- VUM.unsafeRead info _dequeBackPos
  VUM.unsafeWrite info _dequeBackPos (b + n)
  VU.unsafeCopy (VUM.unsafeSlice b n v) vec
{-# INLINE pushBacks #-}

clearDeque :: VU.Unbox a => Deque a -> IO ()
clearDeque (Deque info v) = do
  let o = VUM.length v `quot` 2
  VUM.unsafeWrite info _dequeFrontPos o
  VUM.unsafeWrite info _dequeBackPos o

freezeDeque :: VU.Unbox a => Deque a -> IO (VU.Vector a)
freezeDeque (Deque info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  VU.freeze $ VUM.unsafeSlice f (b - f) v