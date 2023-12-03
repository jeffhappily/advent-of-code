module Year2023.Day1.Part1 (main) where

import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import Data.Text qualified as T

calculateLineCalibration :: Text -> Int
calculateLineCalibration line = case T.foldl' go (Nothing, Nothing) line of
  (Just x, Just y) -> x * 10 + y
  _ -> error "Can't find the answer"
  where
    go :: (Maybe Int, Maybe Int) -> Char -> (Maybe Int, Maybe Int)
    go acc c
      | isDigit c =
          let x = fst acc
              d = digitToInt c
           in case x of
                Nothing -> (Just d, Just d)
                Just _ -> (x, Just d)
      | otherwise = acc

main :: Text -> IO ()
main input = do
  let inputs = T.lines input

  print $ sum . fmap calculateLineCalibration $ inputs