module Year2021.Day3.Part1 (main) where

import Data.List (transpose)
import Data.Semigroup (Sum)
import Data.Text (Text)
import Data.Text qualified as T

main :: Text -> IO ()
main numbers' = do
  let numbers = T.lines numbers'
  let input = T.transpose numbers

  print $ solution input

data Count = Count
  { zero :: Sum Int,
    one :: Sum Int
  }

data Bit = Zero | One

notBit :: Bit -> Bit
notBit Zero = One
notBit One = Zero

commonBit :: Count -> Bit
commonBit c = if c.zero > c.one then Zero else One

rareBit :: Count -> Bit
rareBit = notBit . commonBit

bitsToBin :: [Bit] -> Int
bitsToBin xs = sum $ zipWith go (reverse xs) [0 ..]
  where
    go :: Bit -> Int -> Int
    go Zero _ = 0
    go One i = 2 ^ i

solution :: [Text] -> Int
solution xs =
  let counts = go (Count 0 0) <$> xs
   in gammaRate counts * epsilonRate counts
  where
    go count xs
      | T.null xs = count
      | otherwise =
          let (x, xs') = T.splitAt 1 xs
           in case x of
                "0" -> go (count {zero = count.zero + 1}) xs'
                "1" -> go (count {one = count.one + 1}) xs'
                _ -> go count xs'

gammaRate :: [Count] -> Int
gammaRate = bitsToBin . fmap commonBit

epsilonRate :: [Count] -> Int
epsilonRate = bitsToBin . fmap rareBit
