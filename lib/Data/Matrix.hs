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

nthRandomVector :: Int -> RNG -> IO (VU.Vector Mint)
nthRandomVector sz rng = do
  mvec <- VUM.unsafeNew sz :: IO (VUM.IOVector Mint)
  rep sz $ \i -> do
    x <- nextMint rng
    VUM.unsafeWrite mvec i (bool 1 x (x /= 0))
  VU.unsafeFreeze mvec

determinantOfSparseMatrix :: Int -> RNG -> SparseMatrix -> IO Mint
determinantOfSparseMatrix n rng m = do
  cur <- nthRandomVector n rng
  rep (VUM.length m) $ \i -> VUM.unsafeModify m (\(x, y, v) -> (x, y, v * (cur VU.! y))) i
  ans <- recurrence n rng m
  let ret = ans VU.! (VU.length ans - 1)
  return $ (\res -> bool (-res) res (even res)) $ VU.foldl' (/) ret cur

recurrence :: Int -> RNG -> SparseMatrix -> IO (VU.Vector Mint)
recurrence n rng m = do
  a <- nthRandomVector n rng
  b <- nthRandomVector n rng
  _a <- VU.unsafeThaw a
  _b <- VU.unsafeThaw b
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
-- xorshift
-------------------------------------------------------------------------------
type RNG = VUM.IOVector Word64

getSeed :: IO Word64
getSeed = (* 0x3b47f24a) . fromInteger <$> getCPUTime
{-# NOINLINE getSeed #-}

newRNG :: IO RNG
newRNG = do
  x <- getSeed
  VUM.replicate 1 (8172645463325252 - x)
{-# NOINLINE newRNG #-}

nextWord64 :: RNG -> IO Word64
nextWord64 rng = do
  x <- VUM.unsafeRead rng 0
  let
    y = x .<<. 7 .^. x
    z = y .>>. 9 .^. y
  VUM.unsafeWrite rng 0 z
  return z

nextInt :: RNG -> IO Int
nextInt rng = unsafeCoerce <$> nextWord64 rng

nextMint :: RNG -> IO Mint
nextMint rng = mint <$> nextInt rng

nextDouble :: RNG -> IO Double
nextDouble rng = do
  t <- nextWord64 rng
  let x = 0x3ff .<<. 52 .|. t .>>. 12
  return $! unsafeCoerce @Word64 @Double x - 1.0

nextGauss :: RNG -> Double -> Double -> IO Double
nextGauss rng mu sigma = do
  x <- nextDouble rng
  y <- nextDouble rng
  let z = sqrt (-2.0 * log x) * cos (2.0 * pi * y)
  return $! sigma * z + mu

randomR :: RNG -> Int -> Int -> IO Int
randomR rng l r = (+ l) . flip mod (r - l + 1) <$> nextInt rng

shuffleM :: VUM.Unbox a => RNG -> VUM.IOVector a -> IO ()
shuffleM rng mvec = do
  rev (VUM.length mvec) $ \i -> do
    j <- nextWord64 rng
    VUM.unsafeSwap mvec i (unsafeCoerce $ rem j (unsafeCoerce i + 1))

shuffle :: VU.Unbox a => RNG -> VU.Vector a -> IO (VU.Vector a)
shuffle rng vec = do
  mv <- VU.unsafeThaw vec
  shuffleM rng mv
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