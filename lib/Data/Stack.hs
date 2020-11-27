module Data.Stack where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data StackIO a = StackIO
  { intVarsStackIO  :: !(VUM.IOVector Int)
  , internalStackIO :: !(VUM.IOVector a)
  }

data StackST s a = StackST
  { intVarsStackST  :: !(VUM.STVector s Int)
  , internalStackST :: !(VUM.STVector s a)
  }

_sizeStack :: Int
_sizeStack = 0
{-# INLINE _sizeStack #-}

defaultStackSize :: Int
defaultStackSize = 1024 * 1024
{-# INLINE defaultStackSize #-}

newStackIO :: VUM.Unbox a => Int -> IO (StackIO a)
newStackIO n = StackIO <$> VUM.replicate 1 0 <*> VUM.unsafeNew n

newStackST :: VUM.Unbox a => Int -> ST s (StackST s a)
newStackST n = StackST <$> VUM.replicate 1 0 <*> VUM.unsafeNew n

popStackIO :: VUM.Unbox a => StackIO a -> IO (Maybe a)
popStackIO (StackIO info s) = do
  len <- VUM.unsafeRead info _sizeStack
  if len > 0
    then do
      VUM.unsafeWrite info _sizeStack (len - 1)
      pure <$> VUM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popStackIO #-}

popStackST :: VUM.Unbox a => StackST s a -> ST s (Maybe a)
popStackST (StackST info s) = do
  len <- VUM.unsafeRead info _sizeStack
  if len > 0
    then do
      VUM.unsafeWrite info _sizeStack (len - 1)
      pure <$> VUM.unsafeRead s (len - 1)
    else return Nothing
{-# INLINE popStackST #-}

pushStackIO :: VUM.Unbox a => StackIO a -> a -> IO ()
pushStackIO (StackIO info s) x = do
  len <- VUM.unsafeRead info _sizeStack
  VUM.unsafeWrite s len x
  VUM.unsafeWrite info _sizeStack (len + 1)
{-# INLINE pushStackIO #-}

pushStackST :: VUM.Unbox a => StackST s a -> a -> ST s ()
pushStackST (StackST info s) x = do
  len <- VUM.unsafeRead info _sizeStack
  VUM.unsafeWrite s len x
  VUM.unsafeWrite info _sizeStack (len + 1)
{-# INLINE pushStackST #-}

pushesStackIO :: VUM.Unbox a => StackIO a -> VU.Vector a -> IO ()
pushesStackIO (StackIO info s) vec = do
  len <- VUM.unsafeRead info _sizeStack
  VUM.unsafeWrite info _sizeStack (len + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice len (VU.length vec) s) vec
{-# INLINE pushesStackIO #-}

pushesStackST :: VUM.Unbox a => StackST s a -> VU.Vector a -> ST s ()
pushesStackST (StackST info s) vec = do
  len <- VUM.unsafeRead info _sizeStack
  VUM.unsafeWrite info _sizeStack (len + VU.length vec)
  VU.unsafeCopy (VUM.unsafeSlice len (VU.length vec) s) vec
{-# INLINE pushesStackST #-}

freezeStackIO :: VUM.Unbox a => StackIO a -> IO (VU.Vector a)
freezeStackIO (StackIO info s) = do
  len <- VUM.unsafeRead info _sizeStack
  VU.unsafeFreeze $ VUM.take len s
{-# INLINE freezeStackIO #-}

freezeStackST :: VUM.Unbox a => StackST s a -> ST s (VU.Vector a)
freezeStackST (StackST info s) = do
  len <- VUM.unsafeRead info _sizeStack
  VU.unsafeFreeze $ VUM.take len s
{-# INLINE freezeStackST #-}
