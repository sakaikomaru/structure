{-# LANGUAGE BangPatterns #-}

module Data.CPPVector where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.IORef
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

data CPPVector a = Vec
  { _interanal :: !(VUM.IOVector a)
  , _pointer   :: !(IORef Int)
  }

newVector :: VUM.Unbox a => IO (CPPVector a)
newVector = Vec <$> VUM.unsafeNew defaultVectorSize <*> newIORef 0

defaultVectorSize :: Int
defaultVectorSize = 1024 * 1024

beginIterator :: CPPVector a -> Int
beginIterator _ = 0

endIterator :: VUM.Unbox a => CPPVector a -> IO Int
endIterator (Vec _ ptr) =  succ <$> readIORef ptr

sizeVector :: VUM.Unbox a => CPPVector a -> IO Int
sizeVector (Vec _ ptr) = readIORef ptr

maxSizeVector :: VUM.Unbox a => CPPVector a -> Int
maxSizeVector (Vec info _) = VUM.length info

resizeVector :: VUM.Unbox a => CPPVector a -> Int -> a -> IO ()
resizeVector (Vec info ptr) _newSize padding = do
  let newSize = if _newSize < 0 then 0 else _newSize
  oldSize <- readIORef ptr
  if newSize >= oldSize
    then do
      rep (newSize - oldSize) $ \i -> VUM.unsafeWrite info (oldSize + i) padding
      writeIORef ptr newSize
    else writeIORef ptr newSize

emptyVector :: VUM.Unbox a => CPPVector a -> IO Bool
emptyVector (Vec _ ptr) = (== 0) <$> readIORef ptr

assignVector :: (VU.Unbox a, VUM.Unbox a) => CPPVector a -> VU.Vector a -> IO ()
assignVector (Vec info ptr) ax = do
  let !n = VU.length ax
  rep n $ \i -> VUM.unsafeWrite info i (ax VU.! i)
  writeIORef ptr n

atVector :: VUM.Unbox a => CPPVector a -> Int -> IO (Maybe a)
atVector vec@(Vec info _) k = do
  if k >= maxSizeVector vec 
    then return Nothing
    else pure <$> VUM.unsafeRead info k

frontVector :: VUM.Unbox a => CPPVector a -> IO a
frontVector (Vec info _) = do
  VUM.unsafeRead info 0

backVector :: VUM.Unbox a => CPPVector a -> IO (Maybe a)
backVector (Vec info ptr) = do
  p <- readIORef ptr
  if p == 0
    then return Nothing
    else pure <$> VUM.unsafeRead info (p - 1)

pushBackVector :: VUM.Unbox a => CPPVector a -> a -> IO ()
pushBackVector (Vec info ptr) a = do
  i <- readIORef ptr
  VUM.unsafeWrite info i a
  modifyIORef' ptr succ

popBackVector :: VUM.Unbox a => CPPVector a -> IO ()
popBackVector (Vec _ ptr) = modifyIORef' ptr pred

erasePositionVector :: VUM.Unbox a => CPPVector a -> Int -> IO ()
erasePositionVector (Vec info ptr) idx = do
  p <- readIORef ptr
  rep (p - idx - 1) $ \i -> do
    item <- VUM.unsafeRead info (idx + i + 1)
    VUM.unsafeWrite info (idx + i) item
  modifyIORef ptr pred

eraseVector :: VUM.Unbox a => CPPVector a -> Int -> Int -> IO ()
eraseVector (Vec info ptr) left size = do
  rep size $ \i -> do
    item <- VUM.unsafeRead info (left + size + i)
    VUM.unsafeWrite info (i + left) item
  modifyIORef ptr (subtract size)

swapVector :: VUM.Unbox a => CPPVector a -> CPPVector a -> IO ()
swapVector (Vec info1 ptr1) (Vec info2 ptr2) = do
  p1 <- readIORef ptr1
  p2 <- readIORef ptr2
  let p = max p1 p2
  rep p $ \i -> do
    item1 <- VUM.unsafeRead info1 i
    item2 <- VUM.unsafeRead info2 i
    VUM.unsafeWrite info1 i item2
    VUM.unsafeWrite info2 i item1
  writeIORef ptr1 p2
  writeIORef ptr2 p1
  return ()

clearVector :: VUM.Unbox a => CPPVector a -> IO ()
clearVector (Vec _ ptr) = writeIORef ptr 0

freezeVector :: (VU.Unbox a, VUM.Unbox a) => CPPVector a -> IO (VU.Vector a)
freezeVector (Vec info ptr) = do
  p <- readIORef ptr
  VU.unsafeFreeze $ VUM.unsafeSlice 0 p info

-------------------------------------------------------------------------------
-- for
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (streamG 0 (n - 1) const 0 (+) 1)
{-# INLINE rep #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (streamG 0 n const 0 (+) 1)
{-# INLINE rep' #-}

rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (streamG 1 (n - 1) const 0 (+) 1)
{-# INLINE rep1 #-}

rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (streamG 1 n const 0 (+) 1)
{-# INLINE rep1' #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamRG (n - 1) 0 const 0 (-) 1)
{-# INLINE rev #-}

rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamRG n 0 const 0 (-) 1)
{-# INLINE rev' #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamRG (n - 1) 1 const 0 (-) 1)
{-# INLINE rev1 #-}

rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamRG n 1 const 0 (-) 1)
{-# INLINE rev1' #-}

range :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
range l r = flip VFSM.mapM_ (streamG l r const 0 (+) 1)
{-# INLINE range #-}

rangeR :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rangeR r l = flip VFSM.mapM_ (streamRG r l const 0 (-) 1)
{-# INLINE rangeR #-}

forP :: Monad m => Int -> (Int -> m ()) -> m ()
forP p = flip VFSM.mapM_ (streamG 2 p (^) 2 (+) 1)
{-# INLINE forP #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

forRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forRG r l f p g d = flip VFSM.mapM_ (streamRG r l f p g d)
{-# INLINE forRG #-}

streamG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamG !l !r !f !p !g !d = VFSM.Stream step l
  where
    step x
      | f x p <= r = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamG #-}

streamRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> VFSM.Stream m Int
streamRG !r !l !f !p !g !d = VFSM.Stream step r
  where
    step x
      | f x p >= l = return $ VFSM.Yield x (g x d)
      | otherwise  = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamRG #-}

withBreakIO :: ((r -> ContT r IO b) -> ContT r IO r) -> IO r
withBreakIO = flip runContT pure . callCC
{-# INLINE withBreakIO #-}

withBreakST :: ((r -> ContT r (ST s) b) -> ContT r (ST s) r) -> (ST s) r
withBreakST = flip runContT pure . callCC
{-# INLINE withBreakST #-}