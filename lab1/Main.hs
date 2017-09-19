import System.Random
import System.Clock
import Control.DeepSeq

clocks = [
  Monotonic
  , Realtime
  , ProcessCPUTime
  , ThreadCPUTime
  , MonotonicRaw
  , Boottime
  ]
         
main :: IO ()
main = do
  g <- getStdGen
  sequence $ map (task1 g) clocks
  putStrLn "DONE"

task1 g c = do
  res <- fmap minimum $
         sequence $
         replicate 10 $
         measure c g
  putStrLn $ show c ++ " : " ++ show res
    where
      measure c g = do
        --let inputs = [1..1000 :: Int]
        let inputs = take 1000 (randoms g :: [Int])
        t1 <- inputs `deepseq` getTime c
        let s = sum $ inputs
        t2 <- s `deepseq` getTime c
        let delta = nsec $ diffTimeSpec t1 t2
        return delta


