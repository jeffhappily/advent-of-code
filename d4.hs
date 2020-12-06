{- stack script
    --resolver lts-16.10
    --install-ghc
    --ghc-options -Wall
    --package attoparsec
    --package text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Attoparsec.Text
import qualified Data.Text.IO as T (readFile)

data Field
  = BYR -- ^ Birth Year
  | IYR -- ^ Issue Year
  | EYR -- ^ Expiration Year
  | HGT -- ^ Height
  | HCL -- ^ Hair Color
  | ECL -- ^ Eye Color
  | PID -- ^ Passport ID
  | CID -- ^ Country ID
  deriving Show

alphaNumHash :: Parser Char
alphaNumHash = satisfy $ inClass "a-z0-9#"

byrParser :: Parser Field
byrParser = "byr:" *> many1 alphaNumHash *> pure BYR

iyrParser :: Parser Field
iyrParser = "iyr:" *> many1 alphaNumHash *> pure IYR

eyrParser :: Parser Field
eyrParser = "eyr:" *> many1 alphaNumHash *> pure EYR

hgtParser :: Parser Field
hgtParser = "hgt:" *> many1 alphaNumHash *> pure HGT

hclParser :: Parser Field
hclParser = "hcl:" *> many1 alphaNumHash *> pure HCL

eclParser :: Parser Field
eclParser = "ecl:" *> many1 alphaNumHash *> pure ECL

pidParser :: Parser Field
pidParser = "pid:" *> many1 alphaNumHash *> pure PID

cidParser :: Parser Field
cidParser = "cid:" *> many1 alphaNumHash *> pure CID

fieldParser :: Parser Field
fieldParser = choice
  [ byrParser
  , iyrParser
  , eyrParser
  , hgtParser
  , hclParser
  , eclParser
  , pidParser
  , cidParser
  ]

-- | 8 bit value to indicate the presence of equivalent field
data Passport =
  Passport
    { byr :: Bool
    , iyr :: Bool
    , eyr :: Bool
    , hgt :: Bool
    , hcl :: Bool
    , ecl :: Bool
    , pid :: Bool
    , cid :: Bool
    }
  deriving (Eq, Show)

emptyPassport :: Passport
emptyPassport = Passport False False False False False False False False

passportParser :: Parser Passport
passportParser = do
  fields <- many1 (fieldParser <* space)

  pure $
    foldr (\x z@(Passport {..}) -> 
      -- Toggle field
      case x of
        BYR -> z { byr = not byr }
        IYR -> z { iyr = not iyr }
        EYR -> z { eyr = not eyr }
        HGT -> z { hgt = not hgt }
        HCL -> z { hcl = not hcl }
        ECL -> z { ecl = not ecl }
        PID -> z { pid = not pid }
        CID -> z { cid = not cid }
    ) emptyPassport fields

isPassportValid :: Passport -> Bool
isPassportValid (Passport True True True True True True True _) = True
isPassportValid _ = False

inputParser :: Parser [Passport]
inputParser = many1 (passportParser <* many' space)

main :: IO ()
main = do
  content <- T.readFile "d4-input.txt"
  case parseOnly inputParser content of
    Left err -> error err
    Right p -> do
      print $
       length $ filter isPassportValid p

