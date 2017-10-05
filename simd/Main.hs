{-# LANGUAGE
    MagicHash
  , UnboxedTuples
  , PolyKinds
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -O2 -fllvm -funbox-strict-fields -mavx -mavx2 -mavx512f #-}

module Main where

import GHC.Int
import GHC.Prim
import GHC.Types
import GHC.IO

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import Data.SIMD



distance_simd4 :: VU.Vector Float -> VU.Vector Float -> Float
distance_simd4 v1 v2 = sqrt $ plusHorizontalX4 $ go 0 (VG.length v1'-1)
    where
        v1' = unsafeVectorizeUnboxedX4 v1
        v2' = unsafeVectorizeUnboxedX4 v2
        
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `VG.unsafeIndex` i - v2' `VG.unsafeIndex` i

main :: IO ()
main = do

  let veclen = 16000
  
  let lf1 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
      lf2 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

  let vuf1 = VU.fromList lf1 :: VU.Vector Float
      vuf2 = VU.fromList lf2 :: VU.Vector Float
  deepseq vuf1 $ deepseq vuf2 $ return () 

  distance_simd4 vuf1 vuf2 `deepseq` return ()
  
{-
main :: IO ()
main =
  IO $ \st0 -> case newPinnedByteArray# 16# st0 of
    (# st1, a #) -> case newPinnedByteArray# 16# st1 of
      (# st2, b #) -> case newPinnedByteArray# 16# st2 of
        (# st3, c #) -> case writeIntArray# a 1# 3# st3 of
          st4 -> case writeIntArray# b 1# 4# st4 of
            st5 -> case sumListsSIMD a b c 0# 16# st5 of
              (# st6, r #) -> case readIntArray# r 1# st6 of
                (# st7, i #) -> case unIO (print (I# i)) st7 of
                  (# st8, () #) -> unIO (print "Ok!") st8


sumListsSIMD ::
  MutableByteArray# s ->
  MutableByteArray# s ->
  MutableByteArray# s ->
  Int# ->
  Int# ->
  State# s
  ->
  (# State# s, MutableByteArray# s #)
sumListsSIMD _ _ zs i s st | (I# i) == (I# s) = (# st, zs #)
sumListsSIMD xs ys zs i s st0 =
  case readIntArray# xs i st0 of
    (# st1, x1 #) -> case readIntArray# xs (i +# 1#) st1 of
      (# st2, x2 #) -> case readIntArray# xs (i +# 2#) st2 of
        (# st3, x3 #) -> case readIntArray# xs (i +# 3#) st3 of
          (# st4, x4 #) -> case readIntArray# ys i st4 of
            (# st5, y1 #) -> case readIntArray# ys (i +# 1#) st5 of
              (# st6, y2 #) -> case readIntArray# ys (i +# 2#) st6 of
                (# st7, y3 #) -> case readIntArray# ys (i +# 3#) st7 of
                  (# st8, y4 #) -> case (packInt64X4# (# x1, x2, x3, x4 #)) `plusInt64X4#` (packInt64X4# (# y1, y2, y3, y4 #)) of
    
                    z ->
                      case writeInt64X4Array# zs i z st8 of
                        st9 -> sumListsSIMD xs ys zs (i +# 4#) s st9
  
-}
