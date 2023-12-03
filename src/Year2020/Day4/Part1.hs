{-# LANGUAGE RecordWildCards #-}

module Year2020.Day4.Part1 (main, FieldType (..), Field (..), Passport (..), HeightUnit (..), Height (..), inputParser, heightParser, hairColorParser) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Either (isRight)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T (readFile)
import Text.Read (readMaybe)

data FieldType
  = -- | Birth Year
    BYR
  | -- | Issue Year
    IYR
  | -- | Expiration Year
    EYR
  | -- | Height
    HGT
  | -- | Hair Color
    HCL
  | -- | Eye Color
    ECL
  | -- | Passport ID
    PID
  | -- | Country ID
    CID
  deriving (Eq, Show)

data Field = Field FieldType T.Text
  deriving (Eq, Show)

alphaNumHash :: Char -> Bool
alphaNumHash = inClass "a-z0-9#"

byrParser :: Parser FieldType
byrParser = "byr:" $> BYR

iyrParser :: Parser FieldType
iyrParser = "iyr:" $> IYR

eyrParser :: Parser FieldType
eyrParser = "eyr:" $> EYR

hgtParser :: Parser FieldType
hgtParser = "hgt:" $> HGT

hclParser :: Parser FieldType
hclParser = "hcl:" $> HCL

eclParser :: Parser FieldType
eclParser = "ecl:" $> ECL

pidParser :: Parser FieldType
pidParser = "pid:" $> PID

cidParser :: Parser FieldType
cidParser = "cid:" $> CID

fieldParser :: Parser Field
fieldParser = Field <$> fieldType <*> takeWhile1 alphaNumHash
  where
    fieldType =
      choice
        [ byrParser,
          iyrParser,
          eyrParser,
          hgtParser,
          hclParser,
          eclParser,
          pidParser,
          cidParser
        ]

-- | Passport and its fields
data Passport = Passport
  { byr :: Maybe Field,
    iyr :: Maybe Field,
    eyr :: Maybe Field,
    hgt :: Maybe Field,
    hcl :: Maybe Field,
    ecl :: Maybe Field,
    pid :: Maybe Field,
    cid :: Maybe Field
  }
  deriving (Eq, Show)

emptyPassport :: Passport
emptyPassport = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

passportParser :: Parser Passport
passportParser = do
  fields <- many1 (fieldParser <* try space)

  pure $
    foldr
      ( \x z@Passport {..} ->
          -- Toggle field, to avoid having more than one of the same field
          case x of
            f@(Field BYR _) -> z {byr = maybe (Just f) (const Nothing) byr}
            f@(Field IYR _) -> z {iyr = maybe (Just f) (const Nothing) iyr}
            f@(Field EYR _) -> z {eyr = maybe (Just f) (const Nothing) eyr}
            f@(Field HGT _) -> z {hgt = maybe (Just f) (const Nothing) hgt}
            f@(Field HCL _) -> z {hcl = maybe (Just f) (const Nothing) hcl}
            f@(Field ECL _) -> z {ecl = maybe (Just f) (const Nothing) ecl}
            f@(Field PID _) -> z {pid = maybe (Just f) (const Nothing) pid}
            f@(Field CID _) -> z {cid = maybe (Just f) (const Nothing) cid}
      )
      emptyPassport
      fields

isPassportValid :: Passport -> Bool
isPassportValid (Passport (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) = True
isPassportValid _ = False

data HeightUnit = Cm | In
  deriving (Show)

data Height = Height Int HeightUnit
  deriving (Show)

heightUnitParser :: Parser HeightUnit
heightUnitParser =
  string "cm" $> Cm
    <|> string "in" $> In

heightParser :: Parser Height
heightParser =
  Height <$> decimal <*> heightUnitParser <* endOfInput

hairColorParser :: Parser ()
hairColorParser = char '#' *> count 6 (satisfy alphaNumHash) *> endOfInput

inputParser :: Parser [Passport]
inputParser = many1 (passportParser <* skipSpace)

main :: Text -> IO ()
main content = do
  case parseOnly inputParser content of
    Left err -> error err
    Right p -> do
      print $
        length $
          filter isPassportValid p
