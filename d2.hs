{- stack script
    --resolver lts-16.10
    --install-ghc
    --ghc-options -Wall
    --package attoparsec
    --package listsafe
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Attoparsec.Text
import qualified Data.Text.IO as T (readFile)

data Policy =
  Policy
  { pR    :: Int
  , pL    :: Int
  , pChar :: Char
  }
  deriving Show

data Password =
  Password
  { pwPolicy :: Policy
  , pwString :: String
  }
  deriving Show

policyParser :: Parser Policy
policyParser = do
  pL <- decimal
  _ <- char '-'
  pR <- decimal
  _ <- char ' '
  pChar <- letter

  return Policy{..}

passwordParser :: Parser Password
passwordParser = do
  pwPolicy <- policyParser
  _ <- string ": "
  pwString <- many1 letter

  return Password{..}

inputParser :: Parser [Password]
inputParser = many' $ passwordParser <* endOfLine

isValid :: Password -> Bool
isValid Password {pwPolicy = Policy {..}, ..} = l >= pL && l <= pR
  where
    l = length $ filter (== pChar) pwString

isValid2 :: Password -> Bool
isValid2 Password {pwPolicy = Policy {..}, ..} =
  (first == pChar) /= (second == pChar)
  where
    -- Minus one because index starts from zero where given input starts from one
    first = pwString !! (pL - 1)
    second = pwString !! (pR - 1)

main :: IO ()
main = do
  content <- T.readFile "d2-input.txt"
  case parseOnly inputParser content of
    Left err -> error $ "Error parsing: " <> err
    Right input ->
      let
        l1 = length $ filter isValid input
        l2 = length $ filter isValid2 input
      in
        putStrLn $ "Part 1: " <> show l1 <> "\nPart 2: " <> show l2
