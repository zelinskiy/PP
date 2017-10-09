{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import System.Random
import System.Clock
import Control.DeepSeq
import Numeric
import Data.Int
import Data.Typeable
import Data.List
import Data.Matrix
import System.CPUTime.Rdtsc

main :: IO ()
main = do
  g <- getStdGen
  let c = Monotonic
  mapM_ (\t -> t g c) tasks
--  task4 g c
  putStrLn "OK"

-- +RTS -N4 -s -ls


tasks :: [Task]
tasks =
  [ task1
  , task2
  , task3
  , task4
  , task7
  , task9
  ]

clocks =
  [
  -- Тикает с момента запуска системы.
  -- Значение произвольное (время с запуска,
  -- эпоха юникс и тд). Разрешение 20мс.
    Monotonic
  --Системные часы (в юникс эпохе)
  , Realtime
  --Процессорные часы, связанные с текущим процессом. Тикают с запуска процесса
  , ProcessCPUTime
  --То же, но для потоков
  , ThreadCPUTime
  ]
{-
Ignore this
clock_gettime(CLOCK_MONOTONIC_RAW)       100ns/call
clock_gettime(CLOCK_MONOTONIC)           25ns/call
clock_gettime(CLOCK_REALTIME)            25ns/call
clock_gettime(CLOCK_PROCESS_CPUTIME_ID)  400ns/call
rdtsc (implementation @DavidSchwarz)     600ns/call
-}


type Task = StdGen -> Clock -> IO ()

printTimeSpec :: Clock -> TimeSpec -> IO ()
printTimeSpec c t =
  putStrLn $ show c ++ ": "
  ++ showFFloat Nothing ms "" ++ "ms"
  where ms = fromIntegral (nsec t) / 10^6

-- # TASK 1 #

-- Для функції time визначте дату останнього
-- використання 32 бітного часу. Для цього,
-- наприклад, перетворіть час 0x7FFFFFFF в
-- форматі time_t в формат SystemTime.

-- Что бы это ни было, я могу затo взамен
-- определить просто текущее значение часов

task1 :: Task
task1 g c = do
  putStrLn "\n\nTASK 1\n"
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
task2 :: Task
task2 g c = do
  putStrLn "\n\nTASK 2\n"
  mapM_ (task2' g) clocks

task2' g c = do
  let go a b = do
        let !diff = diffTimeSpec a b
        if diff == TimeSpec 0 0
        then getTime c >>= go a
        else return diff
  !a <- getTime c
  !res <- go a a
  printTimeSpec c res

-- # TASK 3 #

-- Перевірте використання
-- {асемблерних вставок},
-- {функції __rdtsc} та
-- {QueryPerformanceCounter}
-- для визначення часу додавання масиву чисел
-- розміром 1000 елементів.
-- Порівняйте результати[3].
-- [3]: Для виконання цього пункту можна
-- зробити 10 вимірів та обрати мінімальне.

task3 :: Task
task3 g c = do
  putStrLn "\n\nTASK 3\n"
  mapM_ (task3' g) clocks
  task3Rdtsc g

task3Rdtsc g = do
  let measure g = do
        let inputs = take 1000 (randoms g :: [Int])
        t1 <- inputs `deepseq` rdtsc
        let s = sum inputs
        t2 <- s `deepseq` rdtsc
        return $ t1 - t2
  
  res <- fmap minimum $
         sequence $
         replicate 10 $
         measure g
  putStrLn $ "rdtsc: " ++ show res ++ " cycles"
  
task3' g c = do
  let measure g c = do
        let inputs = take 1000 (randoms g :: [Int])
        t1 <- inputs `deepseq` getTime c
        let s = foldl' (+) 0 inputs
        t2 <- s `deepseq` getTime c
        let delta = diffTimeSpec t1 t2
        return delta
  
  res <- fmap minimum $
         sequence $
         replicate 10 $
         measure g c
  printTimeSpec c res
  
    
-- # TASK 4 #

-- Для задачі додавання масиву чисел розміром
-- 100000, 200000, 300000 елементів
-- використайте абсолютний та відносний вимір часу.
-- Для абсолютного виміру використовувати функцію
-- omp_get_wtime. Для відносного визначте
-- кількість циклів додавання , які можна виконати
-- за 2 с для кожного розміру. Вимір виконувати
-- функцією GetTickCount. Порівняйте відношення
-- часу T(200000)/T (100000), T(300000)/T (100000)
-- для обох методів. Зробить висновки по можливості
-- використання відносного виміру часу.

data Task4Mode = Absolute | Relative
  deriving Show

task4 :: Task
task4 g c = do
  putStrLn "\n\nTASK 4\n"
  let c = Monotonic
  a <- addsPerSec c
  mapM_ (\(m,n) -> do
    putStrLn $ show m ++ " measurement for " ++ show n
    t <- task4' m n a g c
    printTimeSpec c t)
    [(m,n) | m <- [Absolute, Relative],
      n <- [100000, 200000, 300000]]

task4' Relative n a _ _ = do
  let ns = round $ fromIntegral n / fromIntegral a * 10^9
  return $ TimeSpec (ns `div` 10^9) ns 
task4' Absolute n _ g c = do
  let inputs = take n (randoms g :: [Int])
  t1 <- inputs `deepseq` getTime c
  let s = foldl' (+) 0 inputs
  t2 <- s `deepseq` getTime c
  return $ diffTimeSpec t1 t2
  
addsPerSec :: Clock -> IO Int64
addsPerSec c = getTime c >>= (\t0 -> run t0 0 t0)
  where
    run t0 n t = do
      let n' = n + 1
      t' <- n' `deepseq` getTime c
      if diffTimeSpec t0 t' >= TimeSpec 2 0
      then return $ n' `div` 2
      else run t0 n' t'

-- # TASK 5 #

-- Складіть функції для обчислення добутку
-- 2-х квадратних матриць.
-- Перша функція не використовує об’єкти,
-- інша використовує.

mmult :: Num a => Matrix a -> Matrix a -> Matrix a 
mmult a b =
  matrix n n (\(i, j) -> foldl' (+) 0
              [a!(i,k) * b!(k,j) | k <- [1..n]])
  where n = nrows a

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

-- Нетрудно видеть, что на каждом шаге внешнего
-- генератора происходит _ вызовов sum.

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

newtype (Num a) => MyMatrix a =
  MkMyMatrix {unMyMatrix :: Matrix a}
  deriving (Show, Eq, NFData)

-- А MyMatrix её экземпляром
instance (Num a) => Semigroup (MyMatrix a) where
  a `splus` b =
    MkMyMatrix $ (unMyMatrix a) `mmult` unMyMatrix b

data Task7Mode = NoObjects | WithObjects

instance Show Task7Mode where
  show NoObjects = "Without objects"
  show WithObjects = "With objects"

task7 :: Task
task7 g c = do
  putStrLn "\n\nTASK 7\n"
  let rs = randomRs (1,100) g :: [Int64]
  let rs0 = repeat (4::Int8)
  mapM_ (\(m, s) -> task7' m rs s c)
    [(m,s) | m <- [NoObjects, WithObjects],
             s <- [128,256,512]]

task7' :: (Typeable a, NFData a, Num a, Show a)
  =>
  Task7Mode -> [a] -> Int -> Clock -> IO ()
task7' mode rs s c = do
  let m1 = fromList s s rs
      rs'= drop (s*s) rs
      m2 = fromList s s rs
  t1 <- (m1,m2) `deepseq` getTime c
  let m3 =
        case mode of
          NoObjects -> m1 `mmult` m2
          WithObjects -> unMyMatrix $
            MkMyMatrix m1 `splus` MkMyMatrix m2
  t2 <- m3 `deepseq` getTime c
  let delta = diffTimeSpec t1 t2
  print mode
  print $ typeOf m3
  putStrLn $ "Array size: " ++ show s
  printTimeSpec c delta
  putStrLn ""

-- # TASK 8 #

-- Задайте режим Release при виконанні додатків
-- і виконайте попередній пункт. Поясніть отримані
-- результати.

-- {-# OPTIONS_GHC -debug #-}
-- Debug = 44.001/43.889/0.110
-- Release = 44.173/44.039/0.132

-- Объяснение: DING-DONG, WELCOME TO THE RICE FIELDS

-- # TASK 9 #

-- Дослідіть вплив типу даних
-- (int8, int16, int32, __int64, float, double)
-- для розміру матриць n = 1024. Отримані
-- результати занести в таблицю. Зробіть
-- висновки по впливу типу даних.

task9 :: Task
task9 g c = do
  putStrLn "\n\nTASK 9\n"
  let s = 128
  task7' NoObjects (randomRs (1,32) g::[Int8  ]) s c
  task7' NoObjects (randomRs (1,32) g::[Int16 ]) s c
  task7' NoObjects (randomRs (1,32) g::[Int32 ]) s c
  task7' NoObjects (randomRs (1,32) g::[Int64 ]) s c
  task7' NoObjects (randomRs (1,32) g::[Float ]) s c
  task7' NoObjects (randomRs (1,32) g::[Double]) s c

-- # TASK 10 #

-- Якщо є технічна можливість, виконайте пункт 9
-- за допомогою засобів Performance для Visual Studio.

-- Есть только ТредСкуп

-- # QUESTONS #

-- 1) В залежності від чого обираються функції
-- для виміру часу.
-- Удобства использования, точности, длительности
-- замера

-- 2) Що таке абсолютний та відносний вимір часу.
-- Абсолютный = omp_get_wtime
-- Относительный = замеряем число однотипных операций -- например за 2 секунды

-- 3) Які функції забезпечують найбільшу точність.

-- 4) Чому не можна використовувати функцію __rdtsc
-- для виміру часу для багатоядерних процесорів.
-- мб тому що на каждом ядре свой тактовый генератор

-- 5) Які методи виміру часу для багатоядерних
-- процесорів ви знаєте?

-- Висновок:
-- 1) Haskell is retarded.
-- 2) I Wish I'd knew any other language(((
