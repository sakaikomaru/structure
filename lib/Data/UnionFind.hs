{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UnionFind where

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Fix
import           Control.Monad.ST
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

type UnionFindIO   = VUM.IOVector Int

type UnionFindST s = VUM.STVector s Int

newUnionFindIO :: Int -> IO UnionFindIO
newUnionFindIO n = VUM.replicate n (-1)
{-# INLINE newUnionFindIO #-}

newUnionFindST :: Int -> ST s (UnionFindST s)
newUnionFindST n = VUM.replicate n (-1)
{-# INLINE newUnionFindST #-}

findUnionFindIO :: UnionFindIO -> Int -> IO Int
findUnionFindIO uf x = go x return
  where
    go !x k = do
      px <- VUM.unsafeRead uf x
      if px < 0
        then k x
        else go px $ \ppx -> do
          VUM.unsafeWrite uf x ppx
          k ppx

findUnionFindST :: UnionFindST s -> Int -> ST s Int
findUnionFindST uf x = go x return
  where
    go !x k = do
      px <- VUM.unsafeRead uf x
      if px < 0
        then k x
        else go px $ \ppx -> do
          VUM.unsafeWrite uf x ppx
          k ppx

sizeUnionFindIO :: UnionFindIO -> Int -> IO Int
sizeUnionFindIO uf = fix $ \loop x -> do
  px <- VUM.unsafeRead uf x
  if px < 0
    then return $! negate px
    else loop px

sizeUnionFindST :: UnionFindST s -> Int -> ST s Int
sizeUnionFindST uf = fix $ \loop x -> do
  px <- VUM.unsafeRead uf x
  if px < 0
    then return $! negate px
    else loop px

uniteUnionFindIO :: UnionFindIO -> Int -> Int -> IO Bool
uniteUnionFindIO uf x y = do
  px <- findUnionFindIO uf x
  py <- findUnionFindIO uf y
  if px == py
    then return False
    else do
      rx <- VUM.unsafeRead uf px
      ry <- VUM.unsafeRead uf py
      if rx < ry
        then do
          VUM.unsafeModify uf (+ ry) px
          VUM.unsafeWrite uf py px
        else do
          VUM.unsafeModify uf (+ rx) py
          VUM.unsafeWrite uf px py
      return True
{-# INLINE uniteUnionFindIO #-}

uniteUnionFindST :: UnionFindST s -> Int -> Int -> ST s Bool
uniteUnionFindST uf x y = do
  px <- findUnionFindST uf x
  py <- findUnionFindST uf y
  if px == py
    then return False
    else do
      rx <- VUM.unsafeRead uf px
      ry <- VUM.unsafeRead uf py
      if rx < ry
        then do
          VUM.unsafeModify uf (+ ry) px
          VUM.unsafeWrite uf py px
        else do
          VUM.unsafeModify uf (+ rx) py
          VUM.unsafeWrite uf px py
      return True
{-# INLINE uniteUnionFindST #-}

sameUnionFindIO :: UnionFindIO -> Int -> Int -> IO Bool
sameUnionFindIO uf x y = (==) `fmap` findUnionFindIO uf x `ap` findUnionFindIO uf y
{-# INLINE sameUnionFindIO #-}

sameUnionFindST :: UnionFindST s -> Int -> Int -> ST s Bool
sameUnionFindST uf x y = (==) `fmap` findUnionFindST uf x `ap` findUnionFindST uf y
{-# INLINE sameUnionFindST #-}

freezeUnionFindIO :: UnionFindIO -> IO (VU.Vector Int)
freezeUnionFindIO = VU.unsafeFreeze

freezeUnionFindST :: UnionFindST s -> ST s (VU.Vector Int)
freezeUnionFindST = VU.unsafeFreeze

countGroupUnionFindIO :: UnionFindIO -> IO Int
countGroupUnionFindIO uf = VU.length . VU.filter (< 0) <$> freezeUnionFindIO uf
{-# INLINE countGroupUnionFindIO #-}

countGroupUnionFindST :: UnionFindST s -> ST s Int
countGroupUnionFindST uf = VU.length . VU.filter (< 0) <$> freezeUnionFindST uf
{-# INLINE countGroupUnionFindST #-}

data UnionFindUndoIO = UFUIO
  { valDataUIO :: VUM.IOVector Int
  , historyUIO :: StackIO (Int, Int)
  }

data UnionFindUndoST s = UFUST
  { valDataUST :: VUM.STVector s Int
  , historyUST :: StackST s (Int, Int) 
  }

newUFUIO :: Int -> IO UnionFindUndoIO
newUFUIO n = UFUIO <$> VUM.replicate n (-1 :: Int) <*> newStackIO defaultStackSize

newUFUST :: Int -> ST s (UnionFindUndoST s)
newUFUST n = UFUST <$> VUM.replicate n (-1 :: Int) <*> newStackST defaultStackSize

findUFUIO :: UnionFindUndoIO -> Int -> IO Int
findUFUIO ufu@(UFUIO info _) k = do
  datak <- VUM.unsafeRead info k
  if datak < 0
    then return k
    else findUFUIO ufu datak

findUFUST :: UnionFindUndoST s -> Int -> ST s Int
findUFUST ufu@(UFUST info _) k = do
  datak <- VUM.unsafeRead info k
  if datak < 0
    then return k
    else findUFUST ufu datak

uniteUFUIO :: UnionFindUndoIO -> Int -> Int -> IO Bool
uniteUFUIO ufu@(UFUIO info his) x y = do
  x'    <- findUFUIO ufu x
  y'    <- findUFUIO ufu y
  datax <- VUM.unsafeRead info x'
  datay <- VUM.unsafeRead info y'
  pushStackIO his (x', datax)
  pushStackIO his (y', datay)
  if x' == y'
    then return False
    else do
      let (xx, yy) = if datax > datay then (y', x') else (x', y')
      datayy <- VUM.unsafeRead info yy
      VUM.unsafeModify info (+ datayy) xx
      VUM.unsafeWrite info yy xx
      return True

uniteUFUST :: UnionFindUndoST s -> Int -> Int -> ST s Bool
uniteUFUST ufu@(UFUST info his) x y = do
  x'    <- findUFUST ufu x
  y'    <- findUFUST ufu y
  datax <- VUM.unsafeRead info x'
  datay <- VUM.unsafeRead info y'
  pushStackST his (x', datax)
  pushStackST his (y', datay)
  if x' == y'
    then return False
    else do
      let (xx, yy) = if datax > datay then (y', x') else (x', y')
      datayy <- VUM.unsafeRead info yy
      VUM.unsafeModify info (+ datayy) xx
      VUM.unsafeWrite info yy xx
      return True

connectedUFUIO :: UnionFindUndoIO -> Int -> Int -> IO Bool
connectedUFUIO ufu x y = liftM2 (==) (findUFUIO ufu x) (findUFUIO ufu y)
{-# INLINE connectedUFUIO #-}

connectedUFUST :: UnionFindUndoST s -> Int -> Int -> ST s Bool
connectedUFUST ufu x y = liftM2 (==) (findUFUST ufu x) (findUFUST ufu y)
{-# INLINE connectedUFUST #-}

sizeUFUIO :: UnionFindUndoIO -> Int -> IO Int
sizeUFUIO ufu@(UFUIO info _) k = do
  findk <- findUFUIO ufu k
  ans <- VUM.unsafeRead info findk
  return (-ans)

sizeUFUST :: UnionFindUndoST s -> Int -> ST s Int
sizeUFUST ufu@(UFUST info _) k = do
  findk <- findUFUST ufu k
  ans <- VUM.unsafeRead info findk
  return (-ans)

undoUFUIO :: UnionFindUndoIO -> IO ()
undoUFUIO (UFUIO info his) = do
  popStackIO his >>= \case
    Nothing     -> return ()
    Just (x, y) -> VUM.unsafeWrite info x y
  popStackIO his >>= \case
    Nothing     -> return ()
    Just (z, w) -> VUM.unsafeWrite info z w

undoUFUST :: UnionFindUndoST s -> ST s ()
undoUFUST (UFUST info his) = do
  popStackST his >>= \case
    Nothing     -> return ()
    Just (x, y) -> VUM.unsafeWrite info x y
  popStackST his >>= \case
    Nothing     -> return ()
    Just (z, w) -> VUM.unsafeWrite info z w

snapshotUFUIO :: UnionFindUndoIO -> IO ()
snapshotUFUIO (UFUIO _ his) = do
  sz <- sizeStackIO his
  when (sz > 0) $ rep sz $ \_ -> void $ popStackIO his

snapshotUFUST :: UnionFindUndoST s -> ST s ()
snapshotUFUST (UFUST _ his) = do
  sz <- sizeStackST his
  when (sz > 0) $ rep sz $ \_ -> void $ popStackST his

rollbackUFUIO :: UnionFindUndoIO -> IO ()
rollbackUFUIO ufu@(UFUIO _ his) = do
  sz <- sizeStackIO his
  when (sz > 0) $ rep sz $ \_ -> undoUFUIO ufu

rollbackUFUST :: UnionFindUndoST s -> ST s ()
rollbackUFUST ufu@(UFUST _ his) = do
  sz <- sizeStackST his
  when (sz > 0) $ rep sz $ \_ -> undoUFUST ufu

data WeightedUnionFindIO a = WUFIO
  { valDataWIO :: VUM.IOVector Int
  , weightWIO  :: VUM.IOVector a
  }

data WeightedUnionFindST s a = WUFST
  { valDataWST :: VUM.STVector s Int
  , weightWST  :: VUM.STVector s a
  }

newWUFIO :: (Eq a, Num a, VUM.Unbox a) => Int -> IO (WeightedUnionFindIO a)
newWUFIO n = WUFIO <$> VUM.replicate n (-1) <*> VUM.unsafeNew n

newWUFST :: (Eq a, Num a, VUM.Unbox a) => Int -> ST s (WeightedUnionFindST s a)
newWUFST n = WUFST <$> VUM.replicate n (-1) <*> VUM.unsafeNew n

findWUFIO :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindIO a -> Int -> IO Int
findWUFIO wuf@(WUFIO val ws) k = do
  valk <- VUM.unsafeRead val k
  if valk < 0
    then return k
    else do
      par     <- findWUFIO wuf valk
      wsdatak <- VUM.unsafeRead ws valk
      VUM.unsafeModify ws (+ wsdatak) k
      VUM.unsafeWrite val k par
      return par

findWUFST :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindST s a -> Int -> ST s Int
findWUFST wuf@(WUFST val ws) k = do
  valk <- VUM.unsafeRead val k
  if valk < 0
    then return k
    else do
      par     <- findWUFST wuf valk
      wsdatak <- VUM.unsafeRead ws valk
      VUM.unsafeModify ws (+ wsdatak) k
      VUM.unsafeWrite val k par
      return par

weightWUFIO :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindIO a -> Int -> IO a
weightWUFIO wuf@(WUFIO _ ws) t = findWUFIO wuf t >> VUM.unsafeRead ws t

weightWUFST :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindST s a -> Int -> ST s a
weightWUFST wuf@(WUFST _ ws) t = findWUFST wuf t >> VUM.unsafeRead ws t

uniteWUFIO :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindIO a -> Int -> Int -> a -> IO Bool
uniteWUFIO wuf@(WUFIO val ws) x y w'' = do
  itemW1<- VUM.unsafeRead ws x
  itemW2<- VUM.unsafeRead ws y
  let w' = w'' + itemW1 - itemW2
  x' <- findWUFIO wuf x
  y' <- findWUFIO wuf y
  if x' == y'
    then return False
    else do
      datax <- VUM.unsafeRead val x'
      datay <- VUM.unsafeRead val y'
      let (xx, yy) = if datax > datay then (y', x') else (x', y')
      let w = if datax > datay then - w' else w'
      datayy <- VUM.unsafeRead val yy
      VUM.unsafeModify val (+ datayy) xx
      VUM.unsafeWrite val yy xx
      VUM.unsafeWrite ws yy w
      return True

uniteWUFST :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindST s a -> Int -> Int -> a -> ST s Bool
uniteWUFST wuf@(WUFST val ws) x y w'' = do
  itemW1<- VUM.unsafeRead ws x
  itemW2<- VUM.unsafeRead ws y
  let w' = w'' + itemW1 - itemW2
  x' <- findWUFST wuf x
  y' <- findWUFST wuf y
  if x' == y'
    then return False
    else do
      datax <- VUM.unsafeRead val x'
      datay <- VUM.unsafeRead val y'
      let (xx, yy) = if datax > datay then (y', x') else (x', y')
      let w = if datax > datay then - w' else w'
      datayy <- VUM.unsafeRead val yy
      VUM.unsafeModify val (+ datayy) xx
      VUM.unsafeWrite val yy xx
      VUM.unsafeWrite ws yy w
      return True

diffWUFIO :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindIO a -> Int -> Int -> IO a
diffWUFIO (WUFIO _ ws) x y = do
  wx <- VUM.unsafeRead ws x
  wy <- VUM.unsafeRead ws y
  return $ wy - wx

diffWUFST :: (Eq a, Num a, VUM.Unbox a) => WeightedUnionFindST s a -> Int -> Int -> ST s a
diffWUFST (WUFST _ ws) x y = do
  wx <- VUM.unsafeRead ws x
  wy <- VUM.unsafeRead ws y
  return $ wy - wx

-------------------------------------------------------------------------------
-- stack
-------------------------------------------------------------------------------
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

sizeStackIO :: VUM.Unbox a => StackIO a -> IO Int
sizeStackIO (StackIO info _) = VUM.unsafeRead info _sizeStack

sizeStackST :: VUM.Unbox a => StackST s a -> ST s Int
sizeStackST (StackST info _) = VUM.unsafeRead info _sizeStack

-------------------------------------------------------------------------------
-- for
-------------------------------------------------------------------------------
rep :: Monad m => Int -> (Int -> m ()) -> m ()
rep n = flip VFSM.mapM_ (stream 0 n)
{-# INLINE rep #-}

rep' :: Monad m => Int -> (Int -> m ()) -> m ()
rep' n = flip VFSM.mapM_ (stream 0 (n + 1))
{-# INLINE rep' #-}

rep1 :: Monad m => Int -> (Int -> m ()) -> m ()
rep1 n = flip VFSM.mapM_ (stream 1 n)
{-# INLINE rep1 #-}

rep1' :: Monad m => Int -> (Int -> m ()) -> m ()
rep1' n = flip VFSM.mapM_ (stream 1 (n + 1))
{-# INLINE rep1' #-}

rev :: Monad m => Int -> (Int -> m ()) -> m ()
rev n = flip VFSM.mapM_ (streamR 0 n)
{-# INLINE rev #-}

rev' :: Monad m => Int -> (Int -> m ()) -> m ()
rev' n = flip VFSM.mapM_ (streamR 0 (n + 1))
{-# INLINE rev' #-}

rev1 :: Monad m => Int -> (Int -> m ()) -> m ()
rev1 n = flip VFSM.mapM_ (streamR 1 n)
{-# INLINE rev1 #-}

rev1' :: Monad m => Int -> (Int -> m ()) -> m ()
rev1' n = flip VFSM.mapM_ (streamR 1 (n + 1))
{-# INLINE rev1' #-}

range :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
range l r = flip VFSM.mapM_ (stream l (r + 1))
{-# INLINE range #-}

rangeR :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
rangeR r l = flip VFSM.mapM_ (streamR l (r + 1))
{-# INLINE rangeR #-}

forStep :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
forStep l r d = flip VFSM.mapM_ (streamStep l r d)
{-# INLINE forStep #-}

forStepR :: Monad m => Int -> Int -> Int -> (Int -> m ()) -> m ()
forStepR r l d = flip VFSM.mapM_ (streamStepR l r d)
{-# INLINE forStepR #-}

forP :: Monad m => Int -> (Int -> m ()) -> m ()
forP p = flip VFSM.mapM_ (streamG 2 p (^) 2 (+) 1)
{-# INLINE forP #-}

forG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forG l r f p g d = flip VFSM.mapM_ (streamG l r f p g d)
{-# INLINE forG #-}

forRG :: Monad m => Int -> Int -> (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> (Int -> m ()) -> m ()
forRG r l f p g d = flip VFSM.mapM_ (streamRG r l f p g d)
{-# INLINE forRG #-}

stream :: Monad m => Int -> Int -> VFSM.Stream m Int
stream !l !r = VFSM.Stream step l
  where
    step x
      | x < r     = return $ VFSM.Yield x (x + 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stream #-}

streamR :: Monad m => Int -> Int -> VFSM.Stream m Int
streamR !l !r = VFSM.Stream step (r - 1)
  where
    step x
      | x >= l = return $ VFSM.Yield x (x - 1)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamR #-}

streamStep :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamStep !l !r !d = VFSM.Stream step l
  where
    step x
      | x <= r    = return $ VFSM.Yield x (x + d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamStep #-}

streamStepR :: Monad m => Int -> Int -> Int -> VFSM.Stream m Int
streamStepR !l !r !d = VFSM.Stream step r
  where
    step x
      | x >= l    = return $ VFSM.Yield x (x - d)
      | otherwise = return VFSM.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] streamStepR #-}

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
