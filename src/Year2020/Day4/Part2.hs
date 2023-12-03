{-# LANGUAGE RecordWildCards #-}

module Year2020.Day4.Part2 (main) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Either (isRight)
import Data.Either.Combinators (rightToMaybe)
import Data.Functor (($>))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T (readFile)
import Text.Read (readMaybe)
import Year2020.Day4.Part1 (Field (..), FieldType (..), Height (..), HeightUnit (..), Passport (..), hairColorParser, heightParser, inputParser)

isPassportValid :: Passport -> Bool
isPassportValid
  ( Passport
      (Just (Field _ byr))
      (Just (Field _ iyr))
      (Just (Field _ eyr))
      (Just (Field _ hgt))
      (Just (Field _ hcl))
      (Just (Field _ ecl))
      (Just (Field _ pid))
      _
    ) =
    and
      [ validByr,
        validIyr,
        validEyr,
        validHgt,
        validHcl,
        validEcl,
        validPid
      ]
    where
      validByr = validateYear byr 1920 2002
      validIyr = validateYear iyr 2010 2020
      validEyr = validateYear eyr 2020 2030
      validHgt = maybe False validateHeight $ rightToMaybe (parseOnly heightParser hgt)
      validHcl = isRight $ parseOnly hairColorParser hcl
      validEcl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      validPid = T.length pid == 9 && isJust (readMaybe (T.unpack pid) :: Maybe Int)

      validateYear :: T.Text -> Int -> Int -> Bool
      validateYear year l u = T.length year == 4 && maybe False (\x -> x >= l && x <= u) (readMaybe (T.unpack year))

      validateHeight (Height h Cm) = h >= 150 && h <= 193
      validateHeight (Height h In) = h >= 59 && h <= 76
isPassportValid _ = False

main :: Text -> IO ()
main content = do
  case parseOnly inputParser content of
    Left err -> error err
    Right p -> do
      print $
        length $
          filter isPassportValid p
