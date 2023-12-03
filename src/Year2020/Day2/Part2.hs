{-# LANGUAGE RecordWildCards #-}

module Year2020.Day2.Part2 (main) where

import Data.Attoparsec.Text
import Data.Text (Text)
import Year2020.Day2.Part1 (Password (..), Policy (..), inputParser)

isValid :: Password -> Bool
isValid Password {pwPolicy = Policy {..}, ..} =
  (first == pChar) /= (second == pChar)
  where
    -- Minus one because index starts from zero where given input starts from one
    first = pwString !! (pL - 1)
    second = pwString !! (pR - 1)

main :: Text -> IO ()
main content = do
  case parseOnly inputParser content of
    Left err -> error $ "Error parsing: " <> err
    Right input ->
      let res = length $ filter isValid input
       in putStrLn $ show res
