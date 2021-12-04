{-# LANGUAGE ViewPatterns #-}

module Day2 (main) where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
import Data.Monoid (Sum (Sum))
import Data.Text.IO qualified as T (readFile)
import GHC.Generics (Generic)
import Generic.Data (Generically (Generically))
import Prelude

data Position = Position
  { hozirontal :: Sum Int
  , depth :: Sum Int
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically Position)

data Command
  = Forward Int
  | Down Int
  | Up Int

commandToPosition :: Command -> Position
commandToPosition = \case
  Forward (Sum -> n) -> Position n mempty
  Down (Sum -> n) -> Position mempty n
  Up (Sum -> n) -> Position mempty (negate <$> n)

positionToAnswer :: Position -> Int
positionToAnswer (Position (Sum x) (Sum y)) = x * y

parseForward :: Parser Command
parseForward = Forward <$> (asciiCI "forward " >> decimal)

parseDown :: Parser Command
parseDown = Down <$> (asciiCI "down " >> decimal)

parseUp :: Parser Command
parseUp = Up <$> (asciiCI "up " >> decimal)

parseCommand :: Parser Command
parseCommand = parseForward <|> parseDown <|> parseUp

parseInput :: Parser [Command]
parseInput = many' $ parseCommand <* endOfLine

main :: IO ()
main = do
  content <- T.readFile "y2021/input/d2.txt"
  case parseOnly parseInput content of
    Left err -> error $ "Error parsing: " <> err
    Right input -> do
      let res = foldMap commandToPosition input
      let ans = positionToAnswer res
      putStr "Part 1: "
      print ans
