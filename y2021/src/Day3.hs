module Day3 (main) where

import Data.List (transpose)
import Data.Semigroup (Sum)
import Prelude

main :: IO ()
main = do
  numbers' <- readFile "y2021/input/d3.txt"
  let numbers = lines numbers'
  let input = transpose numbers

  putStrLn "Part 1: "
  print $ part1 input

data Count = Count
  { zero :: Sum Int
  , one :: Sum Int
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

part1 :: [String] -> Int
part1 xs =
  let counts = go (Count 0 0) <$> xs
   in gammaRate counts * epsilonRate counts
 where
  go res [] = res
  go count (x : xs') =
    case x of
      '0' -> go (count{zero = count.zero + 1}) xs'
      '1' -> go (count{one = count.one + 1}) xs'
      _ -> go count xs'

gammaRate :: [Count] -> Int
gammaRate = bitsToBin . fmap commonBit

epsilonRate :: [Count] -> Int
epsilonRate = bitsToBin . fmap rareBit
