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
import Data.Functor ((<&>))
import qualified Data.Text.IO as T (readFile)
import Data.Maybe (isJust)

type BYR = Int
type IYR = Int
type EYR = Int
type HGT = (Int, String) -- Number and unit
type HCL = String
type ECL = String
type PID = String

data Field
  = BYR BYR -- ^ Birth Year
  | IYR IYR -- ^ Issue Year
  | EYR EYR -- ^ Expiration Year
  | HGT HGT -- ^ Height
  | HCL HCL -- ^ Hair Color
  | ECL ECL -- ^ Eye Color
  | PID PID -- ^ Passport ID
  | CID -- ^ Country ID
  deriving Show

alphaNumHash :: Parser Char
alphaNumHash = satisfy $ inClass "a-z0-9#"

byrParser :: Parser Field
byrParser = "byr:" *> decimal <&> BYR

iyrParser :: Parser Field
iyrParser = "iyr:" *> decimal <&> IYR

eyrParser :: Parser Field
eyrParser = "eyr:" *> decimal <&> EYR

hgtParser :: Parser Field
hgtParser = "hgt:" *> $ HGT . (,) <$> decimal <*> many' letter

hclParser :: Parser Field
hclParser = "hcl:" *> many1 alphaNumHash <&> HCL

eclParser :: Parser Field
eclParser = "ecl:" *> many1 alphaNumHash <&> ECL

pidParser :: Parser Field
pidParser = "pid:" *> many1 digit <&> PID

cidParser :: Parser Field
cidParser = "cid:" *> many1 alphaNumHash *> CID

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

-- | 
data Passport =
  Passport
    { byr :: Maybe BYR
    , iyr :: Maybe IYR
    , eyr :: Maybe EYR
    , hgt :: Maybe HGT
    , hcl :: Maybe HCL
    , ecl :: Maybe ECL
    , pid :: Maybe PID
    }
  deriving (Eq, Show)

emptyPassport :: Passport
emptyPassport = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing

passportParser :: Parser (Maybe Passport)
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

