module Year2021.Day1.Part2 (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Year2021.Day1.Part1 qualified as Part1

main :: Text -> IO ()
main numbers' = do
  let numbers = read . T.unpack <$> T.lines numbers'

  print $ solution numbers

solution :: [Int] -> Int
solution (x : y : xs) =
  Part1.solution $
    zipWith3 (\a b c -> a + b + c) (x : y : xs) (y : xs) xs
solution _ = 0
