module Day1 (main) where

import Data.List (sort)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Prelude

main :: IO ()
main = do
  numbers' <- readFile "y2020/input/d1.txt"
  let numbers = V.fromList . sort . fmap (\x -> read x :: Int) $ words numbers'

  case part1 numbers of
    Nothing -> error "Can't find :'("
    Just (x, y) ->
      putStrLn $
        "Found! The two number is "
          <> show x
          <> " & "
          <> show y
          <> ", and their product is "
          <> show (x * y)

  case part2 numbers of
    Nothing -> error "Can't find :'("
    Just (x, y, z) ->
      putStrLn $
        "Found! The three number is "
          <> show x
          <> " & "
          <> show y
          <> " & "
          <> show z
          <> ", and their product is "
          <> show (x * y * z)

{- | Find a pair (two numbers) in a (sorted) list within a given range that
 adds up to 'expected' by simply traversing simultaneously from both the
 'fromI' and 'toI' index towards middle to find a match until both
 pointers meet, returns 'Nothing' if not found.
-}
findPairWithSumFromTo ::
  -- | Expected sum
  Int ->
  -- | From index
  Int ->
  -- | To index
  Int ->
  -- | Target vector
  Vector Int ->
  Maybe (Int, Int)
findPairWithSumFromTo expected fromI toI xs = go fromI toI
 where
  go startI endI
    -- Avoid index out of bounds
    | startI >= V.length xs || endI >= V.length xs =
      Nothing
    | startI == endI = Nothing
    | sum' < expected = go (startI + 1) endI
    | sum' > expected = go startI (endI - 1)
    | otherwise = Just (start, end)
   where
    start = getVal startI
    end = getVal endI
    sum' = start + end
  getVal :: Int -> Int
  getVal = (V.!) xs

{- | Find a pair (two numbers) in a (sorted) list that adds up to 'expected' by
 simply traversing from both ends of the list towards middle to find a match
 until both pointers meet, returns 'Nothing' if not found
-}
findPairWithSum ::
  -- | Expected sum
  Int ->
  -- | Target vector
  Vector Int ->
  Maybe (Int, Int)
findPairWithSum expected xs = findPairWithSumFromTo expected 0 (V.length xs - 1) xs

-- | Find a pair (two numbers) in a list that adds up to 2020
part1 :: Vector Int -> Maybe (Int, Int)
part1 = findPairWithSum 2020

{- | Find a triplet (three numbers) in a list that adds up to 2020, by looping
 through the list, for every number @a@, find two numbers that adds up to
 @2020 - a@
-}
part2 :: Vector Int -> Maybe (Int, Int, Int)
part2 xs =
  V.ifoldr
    ( \i a b -> case b of
        Nothing ->
          (\(y, z) -> (a, y, z))
            <$> findPairWithSumFromTo (2020 - a) 0 (i - 1) xs
        Just c -> Just c
    )
    Nothing
    xs
