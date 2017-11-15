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
  
