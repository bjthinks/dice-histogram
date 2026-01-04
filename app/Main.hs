module Main where

import Control.Monad.Random.Strict
import Data.List
import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import System.Environment
import System.Exit
import System.IO

d :: Int -> Int -> Rand StdGen Int
d n sides = do
  dice <- sequence $ replicate n $ getRandomR (1,sides)
  return $ sum dice

rollDice :: Int -> Int -> Int -> Rand StdGen [Int]
rollDice n sides count = sequence $ replicate count $ d n sides

totals :: (Int, Int) -> [Int] -> [Int]
totals = totals' 0

totals' :: Int -> (Int, Int) -> [Int] -> [Int]
totals' soFar (lo, hi) (r:rs)
  | lo > hi = []
  | r > lo = soFar : totals' 0 (lo+1, hi) (r:rs)
  | otherwise = totals' (soFar+1) (lo, hi) rs
totals' soFar (lo, hi) [] = soFar : replicate (hi-lo) 0

padInt :: Int -> String
padInt n = replicate (4 - length s) ' ' ++ s
  where s = show n

getNanosSinceEpoch :: IO Integer
getNanosSinceEpoch = do
    (MkSystemTime s ns) <- getSystemTime
    -- Convert seconds to nanoseconds and add the current nanosecond fraction
    return $ toInteger s * 10^(9 :: Int) + toInteger ns

main :: IO ()
main = do
  t <- getNanosSinceEpoch
  args <- getArgs
  (n, sides, count) <- case args of
    [p, q, r] -> return (read p, read q, read r)
    _ -> do sequence_ $ map (hPutStrLn stderr)
              ["Usage: cabal run dice-histogram -- numDice diceSides numTries"
              ,"For example: cabal run dice-histogram 3 6 100000"
              ,"Simulates 100,000 rolls of 3d6"
              ]
            exitFailure
  let gen = mkStdGen $ fromInteger t
      ns = sort $ evalRand (rollDice n sides count) gen
      range = (n, n*sides)
      results = totals range ns
      biggest = maximum results
      barLengths = map (\x -> x * 70 `div` biggest) results
  sequence_ $ [putStrLn (padInt roll ++ " " ++ replicate len '=') |
               (roll, len) <- zip [fst range..snd range] barLengths]
