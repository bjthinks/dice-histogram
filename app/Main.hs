module Main where

import Control.Monad.Random

d :: Int -> Int -> Rand StdGen Int
d n s = do
  rolls <- sequence $ replicate s $ getRandomR (1,n)
  return $ sum rolls

main :: IO ()
main = do
  let gen = mkStdGen 42
  let n = evalRand (3 `d` 6) gen
  print n
