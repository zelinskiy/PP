import System.Random
import System.Clock
import Control.DeepSeq

main :: IO ()
main = do
  task1
  putStrLn "DONE"

task1 = do
  g <- getStdGen  
  let c = Monotonic
  rs <- sequence $ replicate 1000 $ measure c g
  print $ minimum rs
    where
      measure c g = do
        let inputs = take 1000 (randoms g :: [Int])
        t1 <- inputs `deepseq` getTime c
        let s = sum $ inputs
        t2 <- s `deepseq` getTime c
        let delta = nsec $ diffTimeSpec t1 t2
        return delta


