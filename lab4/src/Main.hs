{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.DeepSeq
import System.Clock

clock = Realtime

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs

main :: IO ()
main = do
  forM_ [1..4] $ \n -> do
    putStrLn $ "Hello from thread " ++ show n

(?) :: Bool -> a -> a -> a
True  ? x = const x
False ? _ = id

leubniz :: Integer -> Integer -> Double
leubniz from to =
  (sum [1  / (2 * fromIntegral i + 1) * (odd i ? 1 $ (-1))
  | i <- [from..to]]) * 4

printTimeDiff msg t1 t2 =
  putStrLn $ msg ++ ": " ++ show t
  where t = fromIntegral (sec t2 - sec t1)
          + fromIntegral (nsec t2 - nsec t1) / 10^9

task6 :: Integer -> IO ()
task6 n = do
  let t = 4
  !t1 <- getTime clock
  forM_ [0..t-1] (\i -> forkIO $ undefined)
  let !res = leubniz 1 n
  !t2 <- getTime clock
  printTimeDiff "Control.Concurrent leubniz" t1 t2
