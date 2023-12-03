{-# LANGUAGE ViewPatterns #-}

module Year2021.Day2.Part2 (main) where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
import Data.List (foldl')
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import Data.Text.IO qualified as T (readFile)
import GHC.Generics (Generic)
import Generic.Data (Generically (Generically))
import Year2021.Day2.Part1 (Command (..), Position (..), parseInput, positionToAnswer)

-- | Part 2
mergeCommand :: Position -> Command -> Position
mergeCommand pos@(Position _ _ aim') c =
  let newPos = case c of
        Forward (Sum -> n) -> Position n (aim' * n) mempty
        Down (Sum -> n) -> Position mempty mempty n
        Up (Sum -> n) -> Position mempty mempty (negate <$> n)
   in pos <> newPos

main :: Text -> IO ()
main content = do
  case parseOnly parseInput content of
    Left err -> error $ "Error parsing: " <> err
    Right input -> do
      let res = foldl' mergeCommand mempty input
      let ans = positionToAnswer res
      print ans
