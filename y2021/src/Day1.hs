module Day1 (main) where

import Data.List (foldl')
import Prelude

main :: IO ()
main = do
  numbers' <- readFile "y2021/input/d1.txt"
  let numbers = read <$> lines numbers'

  putStr "Part 1: "
  print $ part1 numbers
  putStr "Part 2: "
  print $ part2 numbers

-- | Find count of numbers that increases from the previous in the list
part1 :: [Int] -> Int
part1 [] = 0
part1 (x : xs) = snd $ foldl' go (x, 0) xs
 where
  go :: (Int, Int) -> Int -> (Int, Int)
  go (prev, n) x' =
    ( x'
    , if x' > prev then n + 1 else n
    )

part2 :: [Int] -> Int
part2 (x : y : xs) =
  part1 $
    zipWith3 (\a b c -> a + b + c) (x : y : xs) (y : xs) xs
part2 _ = 0
