module Year2021.Day1.Part1 (main, solution) where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T

main :: Text -> IO ()
main numbers' = do
  let numbers = read . T.unpack <$> T.lines numbers'

  print $ solution numbers

-- | Find count of numbers that increases from the previous in the list
solution :: [Int] -> Int
solution [] = 0
solution (x : xs) = snd $ foldl' go (x, 0) xs
  where
    go :: (Int, Int) -> Int -> (Int, Int)
    go (prev, n) x' =
      ( x',
        if x' > prev then n + 1 else n
      )
