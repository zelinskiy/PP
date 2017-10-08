{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--{-# OPTIONS_GHC -Wall #-}

import System.Random
import System.Clock
import Control.DeepSeq
import Data.Array
import Numeric
import Data.Int
import Data.Typeable
import Data.List

clocks = [
  Monotonic
  , Realtime
  , ProcessCPUTime
  , ThreadCPUTime
  , MonotonicRaw
  , Boottime
  ]

printTimeSpec :: Clock -> TimeSpec -> IO ()
printTimeSpec c t =
  putStrLn $ show c ++ ": "
  ++ showFFloat Nothing ms "" ++ "ms"
  where ms = fromIntegral (nsec t) / 10^6
    

main :: IO ()
main = do
  g <- getStdGen
  
  task7NoRandom g Realtime
  putStrLn "OK"

-- # TASK 1 #

-- Для функції time визначте дату останнього
-- використання 32 бітного часу. Для цього,
-- наприклад, перетворіть час 0x7FFFFFFF в
-- форматі time_t в формат SystemTime.

-- Что бы это ни было, я могу затo взамен
-- определить просто текущее значение часов

task1 :: RandomGen r => r -> Clock -> IO ()
task1 g c = do
  !t <- getTime c
  printTimeSpec c t

-- # TASK 2 #

-- Експериментально визначте точність обчислення
-- часу при використанні функцій
-- {GetSystemTimePreciseAsFileTime},
-- {GetTickCount}
-- обчислення часу[2].
-- [2]: Для виконання цього пункту можна виконати
-- замір часу, а потім в циклі виконувати наступний
-- замір до тих пір, поки результат не зміниться.
-- Різниця в замірах

-- Порівняйте з даними документації.

-- Зробить висновки по доцільності
-- використанню різних функцій.

-- | Меряем время a, затем пока a /= b меряем b
task2 :: Clock -> IO ()
task2 c = do
  let go a b = do
        let !diff = diffTimeSpec a b
        if diff == TimeSpec 0 0
        then getTime c >>= go a
        else return diff
  !a <- getTime c
  !res <- go a a
  printTimeSpec c res


-- Перевірте використання
-- {асемблерних вставок},
-- {функції __rdtsc} та
-- {QueryPerformanceCounter}
-- для визначення часу додавання масиву чисел
-- розміром 1000 елементів.
-- Порівняйте результати[3].
-- [3]: Для виконання цього пункту можна
-- зробити 10 вимірів та обрати мінімальне.


-- ! Здесь явно рандомные числа не меняются
task3 :: RandomGen r => r -> Clock -> IO ()
task3 g c = do
  res <- fmap minimum $
         sequence $
         replicate 10 $
         measure
  printTimeSpec c res
    where
      measure = do
        let inputs = take 1000 (randoms g :: [Int])
        t1 <- inputs `deepseq` getTime c
        let s = sum $ inputs
        t2 <- s `deepseq` getTime c
        let delta = diffTimeSpec t1 t2
        return delta

-- # TASK 5 #

-- Складіть функції для обчислення добутку
-- 2-х квадратних матриць.
-- Перша функція не використовує об’єкти,
-- інша використовує.

mmult :: (Ix i, Num a) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a 
mmult a b = array ((x0,y1),(x0',y1')) c
  where
    ((x0,x1),(x0',x1')) = bounds a
    ((y0,y1),(y0',y1')) = bounds b
    ir = range (x0,x0')
    jr = range (y1,y1')
    kr = range (x1,x1')
    c = [((i,j), foldl' (+) 0 [a!(i,k) * b!(k,j) | k <- kr]) | i <- ir, j <- jr]

-- # TASK 6 #

-- Визначте формулу для обчислення O(n).

-- По сути O(n^3), где n - размер матрицы
-- Никаких рекурсивных вызовов нет (они под капотом).
-- Вся работа происходит в последней строчке
-- [((i,j), sum [x!(i,k) * y!(k,j) | k <- kr])
--   | i <- ir, j <- jr]

-- Которая по сути прямая трансляция наивной формулы.
-- Сравните с латехом:
-- $c_{ij} = \sum_{k=1}^{n}{a_{ik}b_{kj}}$

-- Нетрудно видеть, что на каждом шаге внешнего генератора происходит _ вызовов sum.

-- # TASK 7 #

-- Перевірте експериментально цю формулу для
-- різних n у разі невикористання та використання
-- об’єктів. Поясніть отримані результати
-- для матриць ромiром 512, 1024, 2048

-- Какие обьекты в хаскеле, спросите вы?
-- На ПЗ я уточнил, что обьекты
-- - это таки экземпляры класса.

-- Пусть классом будет полугруппа
class Semigroup s where
  splus :: s -> s -> s

newtype (Ix i, Num a) => Matrix i a =
  MkMatrix {unMatrix :: (Array (i,i) a)}
  deriving (Show, Eq, NFData)

instance (Ix i, Num a) => Semigroup (Matrix i a) where
  a `splus` b =
    MkMatrix $ unMatrix a `mmult` unMatrix b

test0 =
  (MkMatrix $ listArray ((0,0),(1,1)) (repeat 5))
  `splus`
  (MkMatrix $ listArray ((0,0),(1,1)) (repeat 10))

data Task7Mode = NoObjects | WithObjects

instance Show Task7Mode where
  show NoObjects = "Without objects"
  show WithObjects = "With objects"

task7NoRandom g c = 
  mapM_ (\(m,s) -> do
    task7' m (repeat 13 :: [Int64]) s c
    task7' m (repeat 13 :: [Int32]) s c
    task7' m (repeat 13 :: [Int16]) s c
    task7' m (repeat 13 :: [Int8 ]) s c)
  [(m, fromIntegral (2^s)) |
   m <- [NoObjects, WithObjects],
   s <- [4..11]]

task7 :: RandomGen r => r -> Clock -> IO ()
task7 g c = 
  mapM_ (\(ob,s) -> do
    task7' ob (randomRs (1,32) g :: [Int64]) s c
    task7' ob (randomRs (1,32) g :: [Int32]) s c
    task7' ob (randomRs (1,32) g :: [Int16]) s c
    task7' ob (randomRs (1,32) g :: [Int8 ]) s c)
  [(ob, fromIntegral (2^s)) |
   ob <- [NoObjects, WithObjects],
   s <- [4..9]]

task7' :: (Typeable a, NFData a, Integral a, Show a) =>
  Task7Mode -> [a] -> Int -> Clock -> IO ()
task7' mode rs s c = do
  let m1 = listArray ((0,0),(s-1,s-1)) rs
      rs' = drop (s*s) rs
      m2 = listArray ((0,0),(s-1,s-1)) rs'
  t1 <- (m1,m2) `deepseq` getTime c
  let m3 = case mode of
        NoObjects ->
          m1 `mmult` m2
        WithObjects ->
          unMatrix $ MkMatrix m1 `splus` MkMatrix m2
  t2 <- m3 `deepseq` getTime c
  let delta = diffTimeSpec t1 t2
  print mode
  print $ typeOf m3
  putStrLn $ "Array size: " ++ show s
  printTimeSpec c delta
  putStrLn ""
