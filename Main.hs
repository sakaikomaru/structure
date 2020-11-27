module Main where

import           Data.CPPVector
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  vec <- newVector :: IO (CPPVector Int)
  assignVector vec (VU.fromList ([1,2,3,4,5,6,7,8,9] :: [Int]))
  resizeVector vec (-2) 0
  print =<< freezeVector vec