module Day1 (main) where

import Data.List (foldl')
import Prelude

main :: IO ()
main = do
  numbers' <- readFile "y2021/input/d1.txt"
  let numbers = read <$> lines numbers'

  putStr "Part 1: "
  print $ part1 numbers

-- | Find count of numbers that increases from the previous in ths list
part1 :: [Int] -> Int
part1 [] = 0
part1 (x : xs) = snd $ foldl' go (x, 0) xs
 where
  go :: (Int, Int) -> Int -> (Int, Int)
  go (prev, n) x' = (x', if x' > prev then n + 1 else n)
