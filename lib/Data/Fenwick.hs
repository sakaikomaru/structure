{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Fenwick where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.ST
import           Data.Bits
import           Data.Word
import           GHC.Exts
import           Unsafe.Coerce
import qualified GHC.Integer.GMP.Internals         as GMP
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM

type FenwickIO = VUM.IOVector Int

type FenwickST s = VUM.STVector s Int

newFenwickIO :: Int -> IO FenwickIO
newFenwickIO n = VUM.replicate (n + 1) 0
{-# INLINE newFenwickIO #-}

newFenwickST :: Int -> ST s (FenwickST s)
newFenwickST n = VUM.replicate (n + 1) 0
{-# INLINE newFenwickST #-}

buildFenwickIO :: VU.Vector Int -> IO FenwickIO
buildFenwickIO vec = do
  let n = VU.length vec
  fenwick <- VUM.unsafeNew (n + 1)
  VUM.unsafeWrite fenwick 0 0
  VU.unsafeCopy (VUM.tail fenwick) vec
  flip fix 1 $ \loop !i -> when (i <= n) $ do
    let j = i + (i .&. (-i))
    when (j <= n) $ do
      fenwicki <- VUM.unsafeRead fenwick i
      VUM.unsafeModify fenwick (+ fenwicki) j
    loop (i + 1)
  return fenwick
{-# INLINE buildFenwickIO #-}

buildFenwickST :: VU.Vector Int -> ST s (FenwickST s)
buildFenwickST vec = do
  let n = VU.length vec
  fenwick <- VUM.unsafeNew (n + 1)
  VUM.unsafeWrite fenwick 0 0
  VU.unsafeCopy (VUM.tail fenwick) vec
  flip fix 1 $ \loop !i -> when (i <= n) $ do
    let j = i + (i .&. (-i))
    when (j <= n) $ do
      fenwicki <- VUM.unsafeRead fenwick i
      VUM.unsafeModify fenwick (+ fenwicki) j
    loop (i + 1)
  return fenwick
{-# INLINE buildFenwickST #-}

infixl 9 +++!, +++!!, +++$, +++$$

-- | 0-indexed
(+++!!) :: FenwickIO -> Int -> IO Int
bit +++!! i = bit +++! (i + 1)

-- | 0-indexed
(+++$$) :: FenwickST s -> Int -> ST s Int
bit +++$$ i = bit +++$ (i + 1)

-- | 1-indexed
(+++!) :: FenwickIO -> Int -> IO Int
(+++!) bit = go 0
  where
    go !acc !i
      | i > 0 = do
          xi <- VUM.unsafeRead bit i
          go (acc + xi) (i - (i .&. (-i)))
      | otherwise = return acc

-- | 1-indexed
(+++$) :: FenwickST s -> Int -> ST s Int
(+++$) bit = go 0
  where
    go !acc !i
      | i > 0 = do
          xi <- VUM.unsafeRead bit i
          go (acc + xi) (i - (i .&. (-i)))
      | otherwise = return acc

-- | 0-indexed
incFenwickIO' :: FenwickIO -> Int -> Int -> IO ()
incFenwickIO' bit key val = flip fix (key + 1) $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify bit (+ val) i
    loop $ i + (i .&. (-i))
  where !n = VUM.length bit
{-# INLINE incFenwickIO' #-}

-- | 0-indexed
incFenwickST' :: FenwickST s -> Int -> Int -> ST s ()
incFenwickST' bit key val = flip fix (key + 1) $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify bit (+ val) i
    loop $ i + (i .&. (-i))
  where !n = VUM.length bit
{-# INLINE incFenwickST' #-}

-- | 1-indexed
incFenwickIO :: FenwickIO -> Int -> Int -> IO ()
incFenwickIO bit key val = flip fix key $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify bit (+ val) i
    loop $ i + (i .&. (-i))
  where !n = VUM.length bit
{-# INLINE incFenwickIO #-}

-- | 1-indexed
incFenwickST :: FenwickST s -> Int -> Int -> ST s ()
incFenwickST bit key val = flip fix key $ \loop !i -> do
  when (i < n) $ do
    VUM.unsafeModify bit (+ val) i
    loop $ i + (i .&. (-i))
  where !n = VUM.length bit
{-# INLINE incFenwickST #-}

-- | 1-indexed @[l, r)@
sumFromToIO :: FenwickIO -> Int -> Int -> IO Int
sumFromToIO bit l r = (-) <$> bit +++! (r - 1) <*> bit +++! (l - 1)
{-# INLINE sumFromToIO #-}

-- | 1-indexed @[l, r)@
sumFromToST :: FenwickST s -> Int -> Int -> ST s Int
sumFromToST bit l r = (-) <$> bit +++$ (r - 1) <*> bit +++$ (l - 1)
{-# INLINE sumFromToST #-}

-- | 1-indexed @[l, r]@
sumFromToIO' :: FenwickIO -> Int -> Int -> IO Int
sumFromToIO' bit l r = (-) <$> bit +++! r <*> bit +++! (l - 1)
{-# INLINE sumFromToIO' #-}

-- | 1-indexed @[l, r]@
sumFromToST' :: FenwickST s -> Int -> Int -> ST s Int
sumFromToST' bit l r = (-) <$> bit +++$ r <*> bit +++$ (l - 1)
{-# INLINE sumFromToST' #-}

-- | 0-indexed
readFenwickIO' :: FenwickIO -> Int -> IO Int
readFenwickIO' bit i = (-) <$> bit +++! (i + 1) <*> bit +++! i
{-# INLINE readFenwickIO' #-}

-- | 0-indexed
readFenwickST' :: FenwickST s -> Int -> ST s Int
readFenwickST' bit i = (-) <$> bit +++$ (i + 1) <*> bit +++$ i
{-# INLINE readFenwickST' #-}

-- | 1-indexed
readFenwickIO :: FenwickIO -> Int -> IO Int
readFenwickIO bit i = (-) <$> bit +++! i <*> bit +++! (i - 1)
{-# INLINE readFenwickIO #-}

-- | 1-indexed
readFenwickST :: FenwickST s -> Int -> ST s Int
readFenwickST bit i = (-) <$> bit +++$ i <*> bit +++$ (i - 1)
{-# INLINE readFenwickST #-}

-- | 0-indexed
writeFenwickIO' :: FenwickIO -> Int -> Int -> IO ()
writeFenwickIO' bit i x = readFenwickIO' bit i >>= incFenwickIO' bit i . (x - )
{-# INLINE writeFenwickIO' #-}

-- | 0-indexed
writeFenwickST' :: FenwickST s -> Int -> Int -> ST s ()
writeFenwickST' bit i x = readFenwickST' bit i >>= incFenwickST' bit i . (x - )
{-# INLINE writeFenwickST' #-}

-- | 1-indexed
writeFenwickIO :: FenwickIO -> Int -> Int -> IO ()
writeFenwickIO bit i x = readFenwickIO bit i >>= incFenwickIO bit i . (x - )
{-# INLINE writeFenwickIO #-}

-- | 1-indexed
writeFenwickST :: FenwickST s -> Int -> Int -> ST s ()
writeFenwickST bit i x = readFenwickST bit i >>= incFenwickST bit i . (x - )
{-# INLINE writeFenwickST #-}

-- | 1-indexed
findMinIndexGTIO :: FenwickIO -> Int -> IO Int
findMinIndexGTIO bit w0
  | w0 <= 0   = return 0
  | otherwise = do
    let n = VUM.length bit
    wmax <- bit +++! n
    if w0 > wmax
      then return (n + 1)
      else go w0 (floorPow2 n) 0 n
  where
    go !w !step !i !m
      | step == 0 = return (i + 1)
      | otherwise = do
        if i + step < m
          then do
            u <- VUM.unsafeRead bit (i + step)
            if u < w
              then go (w - u) (step .>>. 1) (i + step) m
              else go w (step .>>. 1) i m
          else go w (step .>>. 1) i m
{-# INLINE findMinIndexGTIO #-}

-- | 1-indexed
findMinIndexGTST :: FenwickST s -> Int -> ST s Int
findMinIndexGTST bit w0
  | w0 <= 0   = return 0
  | otherwise = do
    let n = VUM.length bit
    wmax <- bit +++$ n
    if w0 > wmax
      then return (n + 1)
      else go w0 (floorPow2 n) 0 n
  where
    go !w !step !i !m
      | step == 0 = return (i + 1)
      | otherwise = do
        if i + step < m
          then do
            u <- VUM.unsafeRead bit (i + step)
            if u < w
              then go (w - u) (step .>>. 1) (i + step) m
              else go w (step .>>. 1) i m
          else go w (step .>>. 1) i m
{-# INLINE findMinIndexGTST #-}

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