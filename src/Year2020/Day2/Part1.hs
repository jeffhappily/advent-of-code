{-# LANGUAGE RecordWildCards #-}

module Year2020.Day2.Part1 (Password (..), Policy (..), main, inputParser) where

import Data.Attoparsec.Text
import Data.Text (Text)

data Policy = Policy
  { pR :: Int,
    pL :: Int,
    pChar :: Char
  }
  deriving (Show)

data Password = Password
  { pwPolicy :: Policy,
    pwString :: String
  }
  deriving (Show)

policyParser :: Parser Policy
policyParser = do
  pL <- decimal
  _ <- char '-'
  pR <- decimal
  _ <- char ' '
  pChar <- letter

  return Policy {..}

passwordParser :: Parser Password
passwordParser = do
  pwPolicy <- policyParser
  _ <- string ": "
  pwString <- many1 letter

  return Password {..}

inputParser :: Parser [Password]
inputParser = many' $ passwordParser <* endOfLine

isValid :: Password -> Bool
isValid Password {pwPolicy = Policy {..}, ..} =
  l >= pL && l <= pR
  where
    l = length $ filter (== pChar) pwString

main :: Text -> IO ()
main content = do
  case parseOnly inputParser content of
    Left err -> error $ "Error parsing: " <> err
    Right input ->
      let res = length $ filter isValid input
       in putStrLn $ show res
