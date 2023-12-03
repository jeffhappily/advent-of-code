{-# LANGUAGE ViewPatterns #-}

module Year2021.Day2.Part1 (main, Position (..), Command (..), parseInput, positionToAnswer) where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
import Data.List (foldl')
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import Data.Text.IO qualified as T (readFile)
import GHC.Generics (Generic)
import Generic.Data (Generically (Generically))

data Position = Position
  { hozirontal :: Sum Int,
    depth :: Sum Int,
    aim :: Sum Int
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically Position)

data Command
  = Forward Int
  | Down Int
  | Up Int

positionToAnswer :: Position -> Int
positionToAnswer (Position (Sum x) (Sum y) _) = x * y

parseCommand :: Parser Command
parseCommand =
  Forward <$> (asciiCI "forward " >> decimal)
    <|> Down <$> (asciiCI "down " >> decimal)
    <|> Up <$> (asciiCI "up " >> decimal)

parseInput :: Parser [Command]
parseInput = many' $ parseCommand <* endOfLine

-- | Part 1
commandToPosition :: Command -> Position
commandToPosition = \case
  Forward (Sum -> n) -> Position n mempty mempty
  Down (Sum -> n) -> Position mempty n mempty
  Up (Sum -> n) -> Position mempty (negate <$> n) mempty

main :: Text -> IO ()
main content = do
  case parseOnly parseInput content of
    Left err -> error $ "Error parsing: " <> err
    Right input -> do
      let res = foldMap commandToPosition input
      let ans = positionToAnswer res
      print ans
