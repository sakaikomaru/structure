module Data.Deque where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data DequeIO a = DequeIO
  { dequeVarsIO :: !(VUM.IOVector Int)
  , getDequeIO  :: !(VUM.IOVector a)
  }

data DequeST s a = DequeST
  { dequeVarsST :: !(VUM.STVector s Int)
  , getDequeST  :: !(VUM.STVector s a)
  }

_dequeFrontPos :: Int
_dequeFrontPos = 0

_dequeBackPos :: Int
_dequeBackPos = 1

defaultDequeSize :: Int
defaultDequeSize = 1024 * 1024

newDequeIO :: VU.Unbox a => Int -> IO (DequeIO a)
newDequeIO n = DequeIO <$> VUM.replicate 2 n <*> VUM.unsafeNew (2 * n)

newDequeST :: VU.Unbox a => Int -> ST s (DequeST s a)
newDequeST n = DequeST <$> VUM.replicate 2 n <*> VUM.unsafeNew (2 * n)

lengthDequeIO :: VU.Unbox a => DequeIO a -> IO Int
lengthDequeIO (DequeIO info _) = (-) <$> VUM.unsafeRead info _dequeBackPos <*> VUM.unsafeRead info _dequeFrontPos

lengthDequeST :: VU.Unbox a => DequeST s a -> ST s Int
lengthDequeST (DequeST info _) = (-) <$> VUM.unsafeRead info _dequeBackPos <*> VUM.unsafeRead info _dequeFrontPos

popFrontIO :: VU.Unbox a => DequeIO a -> IO (Maybe a)
popFrontIO (DequeIO info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  if f < b
    then do
      VUM.unsafeWrite info _dequeFrontPos (f + 1)
      pure <$> VUM.unsafeRead v f
    else return Nothing
{-# INLINE popFrontIO #-}

popFrontST :: VU.Unbox a => DequeST s a -> ST s (Maybe a)
popFrontST (DequeST info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  if f < b
    then do
      VUM.unsafeWrite info _dequeFrontPos (f + 1)
      pure <$> VUM.unsafeRead v f
    else return Nothing
{-# INLINE popFrontST #-}

popBackIO :: VU.Unbox a => DequeIO a -> IO (Maybe a)
popBackIO (DequeIO info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  if f < b
    then do
      VUM.unsafeWrite info _dequeBackPos (b - 1)
      pure <$> VUM.unsafeRead v (b - 1)
    else return Nothing
{-# INLINE popBackIO #-}

popBackST :: VU.Unbox a => DequeST s a -> ST s (Maybe a)
popBackST (DequeST info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  if f < b
    then do
      VUM.unsafeWrite info _dequeBackPos (b - 1)
      pure <$> VUM.unsafeRead v (b - 1)
    else return Nothing
{-# INLINE popBackST #-}

pushFrontIO :: VU.Unbox a => DequeIO a -> a -> IO ()
pushFrontIO (DequeIO info v) x = do
  f <- VUM.unsafeRead info _dequeFrontPos
  VUM.unsafeWrite v (f - 1) x
  VUM.unsafeWrite info _dequeFrontPos (f - 1)
{-# INLINE pushFrontIO #-}

pushFrontST :: VU.Unbox a => DequeST s a -> a -> ST s ()
pushFrontST (DequeST info v) x = do
  f <- VUM.unsafeRead info _dequeFrontPos
  VUM.unsafeWrite v (f - 1) x
  VUM.unsafeWrite info _dequeFrontPos (f - 1)
{-# INLINE pushFrontST #-}

pushBackIO :: VU.Unbox a => DequeIO a -> a -> IO ()
pushBackIO (DequeIO info v) x = do
  b <- VUM.unsafeRead info _dequeBackPos
  VUM.unsafeWrite v b x
  VUM.unsafeWrite info _dequeBackPos (b + 1)
{-# INLINE pushBackIO #-}

pushBackST :: VU.Unbox a => DequeST s a -> a -> ST s ()
pushBackST (DequeST info v) x = do
  b <- VUM.unsafeRead info _dequeBackPos
  VUM.unsafeWrite v b x
  VUM.unsafeWrite info _dequeBackPos (b + 1)
{-# INLINE pushBackST #-}

pushFrontsIO :: VU.Unbox a => DequeIO a -> VU.Vector a -> IO ()
pushFrontsIO (DequeIO info v) vec = do
  let n = VU.length vec
  f <- VUM.unsafeRead info _dequeFrontPos
  VUM.unsafeWrite info _dequeFrontPos (f - n)
  VU.unsafeCopy (VUM.unsafeSlice (f - n) n v) vec
{-# INLINE pushFrontsIO #-}

pushFrontsST :: VU.Unbox a => DequeST s a -> VU.Vector a -> ST s ()
pushFrontsST (DequeST info v) vec = do
  let n = VU.length vec
  f <- VUM.unsafeRead info _dequeFrontPos
  VUM.unsafeWrite info _dequeFrontPos (f - n)
  VU.unsafeCopy (VUM.unsafeSlice (f - n) n v) vec
{-# INLINE pushFrontsST #-}

pushBacksIO :: VU.Unbox a => DequeIO a -> VU.Vector a -> IO ()
pushBacksIO (DequeIO info v) vec = do
  let n = VU.length vec
  b <- VUM.unsafeRead info _dequeBackPos
  VUM.unsafeWrite info _dequeBackPos (b + n)
  VU.unsafeCopy (VUM.unsafeSlice b n v) vec
{-# INLINE pushBacksIO #-}

pushBacksST :: VU.Unbox a => DequeST s a -> VU.Vector a -> ST s ()
pushBacksST (DequeST info v) vec = do
  let n = VU.length vec
  b <- VUM.unsafeRead info _dequeBackPos
  VUM.unsafeWrite info _dequeBackPos (b + n)
  VU.unsafeCopy (VUM.unsafeSlice b n v) vec
{-# INLINE pushBacksST #-}

clearDequeIO :: VU.Unbox a => DequeIO a -> IO ()
clearDequeIO (DequeIO info v) = do
  let o = VUM.length v `quot` 2
  VUM.unsafeWrite info _dequeFrontPos o
  VUM.unsafeWrite info _dequeBackPos o

clearDequeST :: VU.Unbox a => DequeST s a -> ST s ()
clearDequeST (DequeST info v) = do
  let o = VUM.length v `quot` 2
  VUM.unsafeWrite info _dequeFrontPos o
  VUM.unsafeWrite info _dequeBackPos o

freezeDequeIO :: VU.Unbox a => DequeIO a -> IO (VU.Vector a)
freezeDequeIO (DequeIO info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  VU.freeze $ VUM.unsafeSlice f (b - f) v

freezeDequeST :: VU.Unbox a => DequeST s a -> ST s (VU.Vector a)
freezeDequeST (DequeST info v) = do
  f <- VUM.unsafeRead info _dequeFrontPos
  b <- VUM.unsafeRead info _dequeBackPos
  VU.freeze $ VUM.unsafeSlice f (b - f) v
