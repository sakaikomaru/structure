{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UnboxedTuples              #-}

module Data.Matrix where

import           Control.Monad.Cont
import           Control.Monad.ST
import           Data.Bits
import           Data.Bool
import           Data.Coerce
import           Data.IORef
import qualified Data.Ratio                        as R
import           Data.STRef.Strict
import           Data.Word
import           GHC.Exts
import           System.CPUTime
import           Unsafe.Coerce
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Fusion.Stream.Monadic as VFSM
import qualified Data.Vector.Generic               as VG
import qualified Data.Vector.Generic.Mutable       as VGM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

-------------------------------------------------------------------------------
-- matrix
-------------------------------------------------------------------------------
type SquareMatrix = VU.Vector Mint

infixr 8 <|^|>
infixr 7 <|#|>
infixl 7 <|*|>
infixl 6 <|+|>, <|-|>

matO :: Int -> SquareMatrix
matO sz = VU.replicate sz (0 :: Mint)

matE :: Int -> SquareMatrix
matE sz = VU.imap(\i _ -> bool 0 1 (i `mod` (sz + 1) == 0)) $ VU.replicate (sz * sz) (0 :: Mint)

buildMatrix :: VU.Vector Int -> SquareMatrix
buildMatrix = VU.map mint
{-# INLINE buildMatrix #-}

(<|+|>) :: SquareMatrix -> SquareMatrix -> SquareMatrix
a <|+|> b = VU.zipWith (+) a b
{-# INLINE (<|+|>) #-}

(<|-|>) :: SquareMatrix -> SquareMatrix -> SquareMatrix
a <|-|> b = VU.zipWith (-) a b
{-# INLINE (<|-|>) #-}

(<|*|>) :: SquareMatrix -> SquareMatrix -> SquareMatrix
a <|*|> b = VU.create $ do
  c <- VUM.unsafeNew m :: ST s (VUM.STVector s Mint)
  rep sz $ \i -> rep sz $ \j -> rep sz $ \k -> VUM.unsafeModify c (+ (a VU.! (i * sz + k)) * (b VU.! (k * sz + j))) (i * sz + j)
  return c
  where
    !m  = VU.length a
    !sz = floor . sqrt . fromIntegral $ m

(<|^|>) :: SquareMatrix -> Int -> SquareMatrix
a <|^|> n
  | n == 1    = a
  | n == 0    = matE sz
  | even n    = z <|*|> z
  | otherwise = a <|*|> (z <|*|> z)
  where
    z   = a <|^|> (n `div` 2)
    !m  = VU.length a
    !sz = floor . sqrt . fromIntegral $ m

(<|#|>) :: Int -> SquareMatrix -> SquareMatrix
n <|#|> a = VU.map (* mint n) a
{-# INLINE (<|#|>) #-}

transposeMat :: SquareMatrix -> SquareMatrix
transposeMat a = VU.create $ do
  let
    !n  = VU.length a
    !sz = floor . sqrt . fromIntegral $ n
  b <- VUM.unsafeNew n :: ST s (VUM.STVector s Mint)
  rep sz $ \i -> rep sz $ \j -> do
    VUM.unsafeWrite b (j * sz + i) (a VU.! (i * sz + j))
  return b

takeNthRow :: Int -> SquareMatrix -> VU.Vector Mint
takeNthRow n a = VU.create $ do
  let
    !m  = VU.length a
    !sz = floor . sqrt . fromIntegral $ m
  b <- VUM.unsafeNew sz :: ST s (VUM.STVector s Mint)
  rep sz $ \i -> VUM.unsafeWrite b i (a VU.! ((n - 1) * sz + i))
  return b

takeNthCol :: Int -> SquareMatrix -> VU.Vector Mint
takeNthCol n a = VU.create $ do
  let
    !m  = VU.length a
    !sz = floor . sqrt . fromIntegral $ m
  b <- VUM.unsafeNew sz :: ST s (VUM.STVector s Mint)
  rep sz $ \i -> VUM.unsafeWrite b i (a VU.! (i * sz + (n - 1)))
  return b

determinant :: Int -> SquareMatrix -> Mint
determinant sz a = runST $ do
  retRef <- newSTRef (1 :: Mint)
  b      <- VU.unsafeThaw a
  withBreakST $ \break -> rep sz $ \i -> do
    c <- lift $ check b (-1) i
    if c == (-1)
      then do
        lift $ writeSTRef retRef 0
        break ()
      else do
        when (c /= i) $ lift $ modifySTRef retRef (*(-1))
        rep sz $ \j -> lift $ VUM.unsafeSwap b (c * sz + j) (i * sz + j)
        itemii <- VUM.unsafeRead b (i * sz + i)
        lift $ modifySTRef retRef (*itemii)
        let inva = (1 :: Mint) / itemii
        range (i + 1) (sz - 1) $ \j -> do
          a0 <- VUM.unsafeRead b (j * sz + i)
          range i (sz - 1) $ \k -> do
            item <- VUM.unsafeRead b (i * sz + k)
            VUM.unsafeModify b (subtract (inva * a0 * item)) (j * sz + k)
  readSTRef retRef
  where
    check :: VUM.STVector s Mint -> Int -> Int -> ST s Int
    check mvec ptr idx = do
      pRef <- newSTRef ptr
      withBreakST $ \break -> range idx (sz - 1) $ \j -> do
        item <- VUM.unsafeRead mvec (j * sz + idx)
        when (item /= 0) $ do
          lift $ writeSTRef pRef j
          break ()
      readSTRef pRef

type SparseMatrix = VUM.IOVector (Int, Int, Mint)

buildSparseMatrix :: VU.Vector (Int, Int, Int) -> IO SparseMatrix
buildSparseMatrix = VU.unsafeThaw . VU.map (\(x, y, z) -> (x, y, mint z))

nthRandomVector :: Int -> MT19937 -> IO (VU.Vector Mint)
nthRandomVector sz rng = do
  mvec <- VUM.unsafeNew sz :: IO (VUM.IOVector Mint)
  rep sz $ \i -> VUM.unsafeWrite mvec i . mint =<< randomR rng 1 (modulus - 1)
  VU.unsafeFreeze mvec

determinantOfSparseMatrix :: Int -> MT19937 -> SparseMatrix -> IO Mint
determinantOfSparseMatrix n rng m = do
  cur <- nthRandomVector n rng
  rep (VUM.length m) $ \i -> VUM.unsafeModify m (\(x, y, v) -> (x, y, v * (cur VU.! y))) i
  ans <- recurrence n rng m
  let res = VU.foldl' (/) (VU.last ans) cur
  if even n
    then return $ res * (- 1)
    else return res

recurrence :: Int -> MT19937 -> SparseMatrix -> IO (VU.Vector Mint)
recurrence n rng m = do
  a <- nthRandomVector n rng
  b <- nthRandomVector n rng
  _a <- VU.unsafeThaw a :: IO (VUM.IOVector Mint)
  _b <- VU.unsafeThaw b :: IO (VUM.IOVector Mint)
  v <- VUM.unsafeNew ((n + 1) .<<. 1) :: IO (VUM.IOVector Mint)
  range 0 ((n + 1) .<<. 1 - 1) $ \i -> do
    tmp <- newIORef (0 :: Mint)
    rep n $ \j -> do
      aj <- VUM.unsafeRead _a j
      bj <- VUM.unsafeRead _b j
      modifyIORef' tmp (+ (aj * bj))
    t <- readIORef tmp
    VUM.unsafeWrite v i t
    nxt <- VUM.replicate n  0 :: IO (VUM.IOVector Mint)
    rep (VUM.length m) $ \e -> do
      (x, y, v) <- VUM.unsafeRead m e
      ay <- VUM.unsafeRead _a y
      VUM.unsafeModify nxt (+ (v * ay)) x 
    VUM.unsafeMove _a nxt
  berlekampMasseyIO =<< VU.unsafeFreeze v 

berlekampMassey :: VU.Vector Mint -> VU.Vector Mint
berlekampMassey s = VU.map (* (-1)) $ VU.create $ do
  let !n = VU.length s
  lRef <- newSTRef (0 :: Int)
  mRef <- newSTRef (0 :: Int)
  b <- VUM.replicate n 0 :: ST s (VUM.STVector s Mint)
  c <- VUM.replicate n 0 :: ST s (VUM.STVector s Mint)
  t <- VUM.replicate n 0 :: ST s (VUM.STVector s Mint)
  bbRef  <- newSTRef (1 :: Mint)
  VUM.unsafeWrite b 0 1
  VUM.unsafeWrite c 0 1
  rep n $ \i -> do
    modifySTRef' mRef succ
    dRef <- newSTRef (s VU.! i)
    l <- readSTRef lRef
    rep1' l $ \j -> do
      cj <- VUM.unsafeRead c j
      modifySTRef' dRef (+ (cj * (s VU.! (i - j))))
    d <- readSTRef dRef
    when (d /= 0) $ do
      VUM.unsafeCopy t c
      bb <- readSTRef bbRef
      let coef = d * ((1 :: Mint) / bb)
      m <- readSTRef mRef
      range m (n - 1) $ \j -> do
        bjm <- VUM.unsafeRead b (j - m)
        VUM.unsafeModify c (subtract (coef * bjm)) j
      when (2 * l <= i) $ do
        modifySTRef' lRef (\ll -> i + 1 - ll)
        VUM.unsafeMove b t
        writeSTRef bbRef d
        writeSTRef mRef 0
  l <- readSTRef lRef
  return $ VUM.unsafeSlice 1 l c

berlekampMasseyIO :: VU.Vector Mint -> IO (VU.Vector Mint)
berlekampMasseyIO s = do
  let !n = VU.length s
  lRef <- newIORef (0 :: Int)
  mRef <- newIORef (0 :: Int)
  b <- VUM.replicate n 0 :: IO (VUM.IOVector Mint)
  c <- VUM.replicate n 0 :: IO (VUM.IOVector Mint)
  t <- VUM.replicate n 0 :: IO (VUM.IOVector Mint)
  bbRef <- newIORef (1 :: Mint)
  VUM.unsafeWrite b 0 1
  VUM.unsafeWrite c 0 1
  rep n $ \i -> do
    modifyIORef' mRef succ
    dRef <- newIORef (s VU.! i)
    l <- readIORef lRef
    rep1' l $ \j -> do
      cj <- VUM.unsafeRead c j
      modifyIORef' dRef (+ (cj * (s VU.! (i - j))))
    d <- readIORef dRef
    when (d /= 0) $ do
      VUM.unsafeCopy t c
      bb <- readIORef bbRef
      let coef = d / bb
      m <- readIORef mRef
      range m (n - 1) $ \j -> do
        bjm <- VUM.unsafeRead b (j - m)
        VUM.unsafeModify c (subtract (coef * bjm)) j
      when (2 * l <= i) $ do
        modifyIORef' lRef (\ll -> i + 1 - ll)
        VUM.unsafeMove b t
        writeIORef bbRef d
        writeIORef mRef 0
  l <- readIORef lRef
  VU.map (*(-1)) <$> VU.unsafeFreeze (VUM.unsafeSlice 1 l c)

-------------------------------------------------------------------------------
-- mersenne twister
-------------------------------------------------------------------------------
_pointer :: Int
_pointer = 312
{-# INLINE _pointer #-}

_lowerMask :: Word64
_lowerMask = 0x7FFFFFFF
{-# INLINE _lowerMask #-}

_upperMask :: Word64
_upperMask = 0xFFFFFFFF80000000
{-# INLINE _upperMask #-}

type MT19937 = VUM.IOVector Word64

newMT19937 :: Word64 -> IO MT19937
newMT19937 seed = do
  mt <- VUM.unsafeNew 313 :: IO MT19937
  VUM.unsafeWrite mt _pointer 0
  VUM.unsafeWrite mt 0 seed
  range 1 311 $ \mti -> do
    item <- VUM.unsafeRead mt (mti - 1)
    let rnd = 0x5851F42D4C957F2D * (item .^. (item .>>. 62)) + unsafeCoerce @Int @Word64 mti
    VUM.unsafeWrite mt mti rnd
  return mt

newRNG :: IO MT19937
newRNG = newMT19937 . (fromInteger :: Integer -> Word64) =<< getCPUTime

shiftAndXor :: Word64 -> Word64
shiftAndXor w0 =
  case w0 .^. ((w0 .>>. 29) .&. 0x5555555555555555) of
    w1 -> case w1 .^. ((w1 .<<. 17) .&. 0x71D67FFFEDA60000) of
      w2 -> case w2 .^. ((w2 .<<. 37) .&. 0xFFF7EEE000000000) of
        w3 -> w3 .^. (w3 .>>. 43)

twist :: MT19937 -> IO ()
twist mt = do
  rep 312 $ \i -> do
    item1 <- VUM.unsafeRead mt i
    item2 <- VUM.unsafeRead mt ((i + 1) `mod` 312)
    let
      x  = (item1 .&. _upperMask) + (item2 .&. _lowerMask)
      xA = x .>>. 1
      xa  = if odd x then xA .^. 0xB5026F5AA96619E9 else xA
    item3 <- VUM.unsafeRead mt ((i + 156) `mod` 312)
    VUM.unsafeWrite mt i (item3 .^. xa)
  VUM.unsafeWrite mt _pointer 0

nextWord64 :: MT19937 -> IO Word64
nextWord64 mt = do
  idx <- VUM.unsafeRead mt _pointer
  when (idx >= 312) $ twist mt
  y <- shiftAndXor <$> VUM.unsafeRead mt (bool (fromIntegral idx) 0 (idx >= 312))
  VUM.unsafeModify mt succ _pointer
  return y

nextInt :: MT19937 -> IO Int
nextInt mt = unsafeCoerce <$> nextWord64 mt

nextMint :: MT19937 -> IO Mint
nextMint rng = mint <$> nextInt rng

nextWord :: MT19937 -> IO Word
nextWord mt = unsafeCoerce <$> nextWord64 mt

nextDouble :: MT19937 -> IO Double
nextDouble mt19937 = do
  t <- nextWord64 mt19937
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word64 @Double x - 1.0

nextGauss :: MT19937 -> Double -> Double -> IO Double
nextGauss mt19937 mu sigma = do
  x <- nextDouble mt19937
  y <- nextDouble mt19937
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

randomR :: MT19937 -> Int -> Int -> IO Int
randomR mt19937 l r = (+ l) . flip mod (r - l + 1) <$> nextInt mt19937

shuffleM :: VUM.Unbox a => MT19937 -> VUM.IOVector a -> IO ()
shuffleM mt19937 mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord64 mt19937
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => MT19937 -> VU.Vector a -> IO (VU.Vector a)
shuffle mt19937 vec = do
  mv <- VU.unsafeThaw vec
  shuffleM mt19937 mv
  VU.unsafeFreeze mv


-------------------------------------------------------------------------------
-- mint
-------------------------------------------------------------------------------
#define MOD 998244353

modulus :: Num a => a
modulus = MOD
{-# INLINE modulus #-}

infixr 8 ^%
infixl 7 *%, /%
infixl 6 +%, -%

(+%) :: Int -> Int -> Int
(I# x#) +% (I# y#) = case x# +# y# of
  r# -> I# (r# -# ((r# >=# MOD#) *# MOD#))
{-# INLINE (+%) #-}
(-%) :: Int -> Int -> Int
(I# x#) -% (I# y#) = case x# -# y# of
  r# -> I# (r# +# ((r# <# 0#) *# MOD#))
{-# INLINE (-%) #-}
(*%) :: Int -> Int -> Int
(I# x#) *% (I# y#) = case timesWord# (int2Word# x#) (int2Word# y#) of
  z# -> case timesWord2# z# im# of
    (# q#, _ #) -> case minusWord# z# (timesWord# q# m#) of
      v#  | isTrue# (geWord# v# m#) -> I# (word2Int# (plusWord# v# m#))
          | otherwise -> I# (word2Int# v#)
  where
    m#  = int2Word# MOD#
    im# = plusWord# (quotWord# 0xffffffffffffffff## m#) 1##
{-# INLINE (*%) #-}
(/%) :: Int -> Int -> Int
(I# x#) /% (I# y#) = go# y# MOD# 1# 0#
  where
    go# a# b# u# v#
      | isTrue# (b# ># 0#) = case a# `quotInt#` b# of
        q# -> go# b# (a# -# (q# *# b#)) v# (u# -# (q# *# v#))
      | otherwise = I# ((x# *# (u# +# MOD#)) `remInt#` MOD#)
{-# INLINE (/%) #-}
(^%) :: Int -> Int -> Int
x ^% n
  | n > 0  = go 1 x n
  | n == 0 = 1
  | otherwise = go 1 (1 /% x) (-n)
  where
    go !acc !y !m
      | m .&. 1 == 0 = go acc (y *% y) (unsafeShiftR m 1)
      | m == 1       = acc *% y
      | otherwise    = go (acc *% y) (y *% y) (unsafeShiftR (m - 1) 1)

newtype Mint = Mint { getMint :: Int }
  deriving newtype (Eq, Ord, Read, Show, Real)

mint :: Integral a => a -> Mint
mint x = fromIntegral $ mod (fromIntegral x) MOD
{-# INLINE mint #-}

mintValidate :: Mint -> Bool
mintValidate (Mint x) = 0 <= x && x < MOD
{-# INLINE mintValidate #-}

instance Bounded Mint where
  minBound = Mint 0
  maxBound = Mint $ modulus - 1

instance Enum Mint where
  toEnum = mint
  fromEnum = coerce

instance Integral Mint where
  quotRem x y = (x / y, x - x / y * y)
  toInteger = coerce (toInteger @Int)

instance Num Mint where
  (+) = coerce (+%)
  (-) = coerce (-%)
  (*) = coerce (*%)
  abs = id
  signum = const (Mint 1)
  fromInteger x = coerce @Int @Mint . fromInteger $ mod x modulus

instance Fractional Mint where
  (/) = coerce (/%)
  fromRational q = fromInteger (R.numerator q) / fromInteger (R.denominator q)

newtype instance VUM.MVector s Mint = MV_Mint (VUM.MVector s Int)
newtype instance VU.Vector Mint = V_Mint (VU.Vector Int)

instance VU.Unbox Mint

instance VGM.MVector VUM.MVector Mint where
  basicLength (MV_Mint v) = VGM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Mint v) = MV_Mint $ VGM.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Mint v1) (MV_Mint v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Mint `fmap` VGM.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Mint v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate n x = MV_Mint `fmap` VGM.basicUnsafeReplicate n (coerce x)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Mint v) i = coerce `fmap` VGM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Mint v) i x = VGM.basicUnsafeWrite v i (coerce x)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Mint v) = VGM.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_Mint v) x = VGM.basicSet v (coerce x)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Mint v1) (MV_Mint v2) = VGM.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Mint v1) (MV_Mint v2) = VGM.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Mint v) n = MV_Mint `fmap` VGM.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance VG.Vector VU.Vector Mint where
  basicUnsafeFreeze (MV_Mint v) = V_Mint `fmap` VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Mint v) = MV_Mint `fmap` VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Mint v) = VG.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Mint v) = V_Mint $ VG.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Mint v) i = coerce `fmap` VG.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Mint mv) (V_Mint v) = VG.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE elemseq #-}

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

-------------------------------------------------------------------------------
-- util
-------------------------------------------------------------------------------
fi :: Int -> Integer
fi = fromIntegral
{-# INLINE fi #-}

fI :: Integer -> Int
fI = fromInteger
{-# INLINE fI #-}

powModInt :: Int -> Int -> Int -> Int
powModInt a b c = fI $ GMP.powModInteger (fi a) (fi b) (fi c)
{-# INLINE powModInt #-}

recipModInt :: Int -> Int -> Int
recipModInt a m = fI $ GMP.recipModInteger (fi a) (fi m)
{-# INLINE recipModInt #-}

infixl 8 .<<., .>>., .>>>.
infixl 6 .^.

(.<<.) :: Bits b => b -> Int -> b
(.<<.) = unsafeShiftL
{-# INLINE (.<<.) #-}

(.>>.) :: Bits b => b -> Int -> b
(.>>.) = unsafeShiftR
{-# INLINE (.>>.) #-}

(.>>>.) :: Int -> Int -> Int
(.>>>.) (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE (.>>>.) #-}

(.^.) :: Bits b => b -> b -> b
(.^.)  = xor
{-# INLINE (.^.)  #-}

clz :: FiniteBits fb => fb -> Int
clz = countLeadingZeros
{-# INLINE clz #-}

ctz :: FiniteBits fb => fb -> Int
ctz = countTrailingZeros
{-# INLINE ctz #-}

encode32x2 :: Int -> Int -> Int
encode32x2 x y = x .<<. 32 .|. y
{-# INLINE encode32x2 #-}

decode32x2 :: Int -> (Int, Int)
decode32x2 xy =
    let !x = xy .>>>. 32
        !y = xy .&. 0xffffffff
    in (x, y)
{-# INLINE decode32x2 #-}

ceilPow2 :: Int -> Int
ceilPow2 n
  | n > 1     = (-1) .>>>. clz (n - 1) + 1
  | otherwise = 1
{-# INLINE ceilPow2 #-}

floorPow2 :: Int -> Int
floorPow2 n
  | n >= 1    = 1 .<<. (63 - clz n)
  | otherwise = 0
{-# INLINE floorPow2 #-}

bitReverse :: Int -> Int
bitReverse
  = unsafeCoerce
  . step 32 0xffffffff00000000 0x00000000ffffffff
  . step 16 0xffff0000ffff0000 0x0000ffff0000ffff
  . step 08 0xff00ff00ff00ff00 0x00ff00ff00ff00ff
  . step 04 0xf0f0f0f0f0f0f0f0 0x0f0f0f0f0f0f0f0f
  . step 02 0xcccccccccccccccc 0x3333333333333333
  . step 01 0xaaaaaaaaaaaaaaaa 0x5555555555555555
  . unsafeCoerce
  where
    step :: Int -> Word64 -> Word64 -> Word64 -> Word64
    step i ml mr = \ !x -> (x .&. ml) .>>. i .|. (x .&. mr) .<<. i
    {-# INLINE step #-}

ctzceilpow2 :: Int -> Int
ctzceilpow2 = countTrailingZeros . ceilPow2
{-# INLINE ctzceilpow2 #-}