module Main where

import           Data.CPPVector
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  vec1 <- newVector :: IO (CPPVector Int)
  vec2 <- newVector :: IO (CPPVector Int)
  assignVector vec1 (VU.fromList ([1,2,3,4,5,6,7,8,9] :: [Int]))
  assignVector vec2 (VU.fromList ([1,4,9] :: [Int]))
  swapVector vec1 vec2
  print =<< freezeVector vec1
  print =<< freezeVector vec2