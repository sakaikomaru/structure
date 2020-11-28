{-# LANGUAGE BangPatterns #-}

module Data.VectorCPP where

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.IORef
import           Data.STRef
import qualified Data.Vector                       as V
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Mutable               as VM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

data VectorIO a = VecIO
  { _internalIO :: !(VUM.IOVector a)
  , _pointer    :: !(IORef Int)
  }

data VectorST s a = VecST
  { _internalST :: !(VUM.STVector s a)
  , _pointerST  :: !(STRef s Int)
  }

defaultVectorSize :: Int
defaultVectorSize = 1024 * 1024
{-# INLINE defaultVectorSize #-}

newVectorIO :: VUM.Unbox a => IO (VectorIO a)
newVectorIO = VecIO <$> VUM.unsafeNew defaultVectorSize <*> newIORef 0

newVectorST :: VUM.Unbox a => ST s (VectorST s a)
newVectorST = VecST <$> VUM.unsafeNew defaultVectorSize <*> newSTRef 0

buildVectorIO :: VUM.Unbox a => Int -> IO (VectorIO a)
buildVectorIO n = VecIO <$> VUM.unsafeNew n <*> newIORef 0

buildVectorST :: VUM.Unbox a => Int -> ST s (VectorST s a)
buildVectorST n = VecST <$> VUM.unsafeNew n <*> newSTRef 0

newMultiDimensionVectorIO :: VUM.Unbox a => [Int] -> IO (VectorIO a)
newMultiDimensionVectorIO xs =
  let !n = product xs
  in VecIO <$> VUM.unsafeNew n <*> newIORef 0

newMultiDimensionVectorST :: VUM.Unbox a => [Int] -> ST s (VectorST s a)
newMultiDimensionVectorST xs =
  let !n = product xs
  in VecST <$> VUM.unsafeNew n <*> newSTRef 0

getIteratorIO :: VUM.Unbox a => VectorIO a -> IO Int
getIteratorIO (VecIO _ ptr) = pred <$> readIORef ptr

getIteratorST :: VUM.Unbox a => VectorST s a -> ST s Int
getIteratorST (VecST _ ptr) = pred <$> readSTRef ptr

sizeVectorIO :: VUM.Unbox a => VectorIO a -> IO Int
sizeVectorIO (VecIO _ ptr) = readIORef ptr

sizeVectorST :: VUM.Unbox a => VectorST s a -> ST s Int
sizeVectorST (VecST _ ptr) = readSTRef ptr

maxSizeVectorIO :: VUM.Unbox a => VectorIO a -> Int
maxSizeVectorIO (VecIO info _) = VUM.length info

maxSizeVectorST :: VUM.Unbox a => VectorST s a -> Int
maxSizeVectorST (VecST info _) = VUM.length info

resizeVectorIO :: VUM.Unbox a => VectorIO a -> Int -> a -> IO ()
resizeVectorIO (VecIO info ptr) _newSize padding = do
  let newSize = if _newSize < 0 then 0 else _newSize
  oldSize <- readIORef ptr
  if newSize >= oldSize
    then do
      rep (newSize - oldSize) $ \i -> VUM.unsafeWrite info (oldSize + i) padding
      writeIORef ptr newSize
    else writeIORef ptr newSize

resizeVectorST :: VUM.Unbox a => VectorST s a -> Int -> a -> ST s ()
resizeVectorST (VecST info ptr) _newSize padding = do
  let newSize = if _newSize < 0 then 0 else _newSize
  oldSize <- readSTRef ptr
  if newSize >= oldSize
    then do
      rep (newSize - oldSize) $ \i -> VUM.unsafeWrite info (oldSize + i) padding
      writeSTRef ptr newSize
    else writeSTRef ptr newSize

emptyVectorIO :: VUM.Unbox a => VectorIO a -> IO Bool
emptyVectorIO (VecIO _ ptr) = (== 0) <$> readIORef ptr

emptyVectorST :: VUM.Unbox a => VectorST s a -> ST s Bool
emptyVectorST (VecST _ ptr) = (== 0) <$> readSTRef ptr

assignVectorIO :: (VU.Unbox a, VUM.Unbox a) => VectorIO a -> VU.Vector a -> IO ()
assignVectorIO (VecIO info ptr) ax = do
  let !n = VU.length ax
  rep n $ \i -> VUM.unsafeWrite info i (ax VU.! i)
  writeIORef ptr n

assignVectorST :: (VU.Unbox a, VUM.Unbox a) => VectorST s a -> VU.Vector a -> ST s ()
assignVectorST (VecST info ptr) ax = do
  let !n = VU.length ax
  rep n $ \i -> VUM.unsafeWrite info i (ax VU.! i)
  writeSTRef ptr n

atVectorIO :: VUM.Unbox a => VectorIO a -> Int -> IO (Maybe a)
atVectorIO vec@(VecIO info _) k = do
  if k >= maxSizeVectorIO vec 
    then return Nothing
    else pure <$> VUM.unsafeRead info k

atVectorST :: VUM.Unbox a => VectorST s a -> Int -> ST s (Maybe a)
atVectorST vec@(VecST info _) k = do
  if k >= maxSizeVectorST vec 
    then return Nothing
    else pure <$> VUM.unsafeRead info k

frontVectorIO :: VUM.Unbox a => VectorIO a -> IO a
frontVectorIO (VecIO info _) = VUM.unsafeRead info 0

frontVectorST :: VUM.Unbox a => VectorST s a -> ST s a
frontVectorST (VecST info _) = VUM.unsafeRead info 0

backVectorIO :: VUM.Unbox a => VectorIO a -> IO (Maybe a)
backVectorIO (VecIO info ptr) = do
  p <- readIORef ptr
  if p == 0
    then return Nothing
    else pure <$> VUM.unsafeRead info (p - 1)

backVectorST :: VUM.Unbox a => VectorST s a -> ST s (Maybe a)
backVectorST (VecST info ptr) = do
  p <- readSTRef ptr
  if p == 0
    then return Nothing
    else pure <$> VUM.unsafeRead info (p - 1)

pushBackVectorIO :: VUM.Unbox a => VectorIO a -> a -> IO ()
pushBackVectorIO (VecIO info ptr) a = do
  i <- readIORef ptr
  VUM.unsafeWrite info i a
  modifyIORef' ptr succ

pushBackVectorST :: VUM.Unbox a => VectorST s a -> a -> ST s ()
pushBackVectorST (VecST info ptr) a = do
  i <- readSTRef ptr
  VUM.unsafeWrite info i a
  modifySTRef' ptr succ

popBackVectorIO :: VUM.Unbox a => VectorIO a -> IO ()
popBackVectorIO (VecIO _ ptr) = modifyIORef' ptr pred

popBackVectorST :: VUM.Unbox a => VectorST s a -> ST s ()
popBackVectorST (VecST _ ptr) = modifySTRef' ptr pred

erasePositionVectorIO :: VUM.Unbox a => VectorIO a -> Int -> IO ()
erasePositionVectorIO (VecIO info ptr) idx = do
  p <- readIORef ptr
  rep (p - idx - 1) $ \i -> do
    item <- VUM.unsafeRead info (idx + i + 1)
    VUM.unsafeWrite info (idx + i) item
  modifyIORef ptr pred

erasePositionVectorST :: VUM.Unbox a => VectorST s a -> Int -> ST s ()
erasePositionVectorST (VecST info ptr) idx = do
  p <- readSTRef ptr
  rep (p - idx - 1) $ \i -> do
    item <- VUM.unsafeRead info (idx + i + 1)
    VUM.unsafeWrite info (idx + i) item
  modifySTRef ptr pred

eraseVectorIO :: VUM.Unbox a => VectorIO a -> Int -> Int -> IO ()
eraseVectorIO (VecIO info ptr) left size = do
  rep size $ \i -> do
    item <- VUM.unsafeRead info (left + size + i)
    VUM.unsafeWrite info (i + left) item
  modifyIORef ptr (subtract size)

eraseVectorST :: VUM.Unbox a => VectorST s a -> Int -> Int -> ST s ()
eraseVectorST (VecST info ptr) left size = do
  rep size $ \i -> do
    item <- VUM.unsafeRead info (left + size + i)
    VUM.unsafeWrite info (i + left) item
  modifySTRef ptr (subtract size)

eraseIfVectorIO :: VUM.Unbox a => VectorIO a -> (a -> Bool) -> IO ()
eraseIfVectorIO vec@(VecIO info ptr) f = do
  p <- readIORef ptr
  rep p $ \i -> do
    item <- VUM.unsafeRead info i
    when (f item) $ erasePositionVectorIO vec i

eraseIfVectorST :: VUM.Unbox a => VectorST s a -> (a -> Bool) -> ST s ()
eraseIfVectorST vec@(VecST info ptr) f = do
  p <- readSTRef ptr
  rep p $ \i -> do
    item <- VUM.unsafeRead info i
    when (f item) $ erasePositionVectorST vec i

swapVectorIO :: VUM.Unbox a => VectorIO a -> VectorIO a -> IO ()
swapVectorIO (VecIO info1 ptr1) (VecIO info2 ptr2) = do
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

swapVectorST :: VUM.Unbox a => VectorST s a -> VectorST s a -> ST s ()
swapVectorST (VecST info1 ptr1) (VecST info2 ptr2) = do
  p1 <- readSTRef ptr1
  p2 <- readSTRef ptr2
  let p = max p1 p2
  rep p $ \i -> do
    item1 <- VUM.unsafeRead info1 i
    item2 <- VUM.unsafeRead info2 i
    VUM.unsafeWrite info1 i item2
    VUM.unsafeWrite info2 i item1
  writeSTRef ptr1 p2
  writeSTRef ptr2 p1

-- TODO unique
-- uniqueVectorIO :: VUM.Unbox a => VectorIO a -> Int -> Int -> IO ()
-- uniqueVectorIO (VecIO info ptr) l r = do
--   undefined
-- 
-- uniqueVectorST :: VUM.Unbox a => VectorST s a -> Int -> Int -> ST s ()
-- uniqueVectorST (VecST info ptr) l r = do
--   undefined

clearVectorIO :: VUM.Unbox a => VectorIO a -> IO ()
clearVectorIO (VecIO _ ptr) = writeIORef ptr 0

clearVectorST :: VUM.Unbox a => VectorST s a -> ST s ()
clearVectorST (VecST _ ptr) = writeSTRef ptr 0

freezeVectorIO :: (VU.Unbox a, VUM.Unbox a) => VectorIO a -> IO (VU.Vector a)
freezeVectorIO (VecIO info ptr) = do
  p <- readIORef ptr
  VU.unsafeFreeze $ VUM.unsafeSlice 0 p info

freezeVectorST :: (VU.Unbox a, VUM.Unbox a) => VectorST s a -> ST s (VU.Vector a)
freezeVectorST (VecST info ptr) = do
  p <- readSTRef ptr
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
