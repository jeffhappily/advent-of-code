module Year2020.Day1.Part1 (main, findPairWithSumFromTo) where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

main :: Text -> IO ()
main numbers' = do
  let numbers = V.fromList . sort . fmap (read . T.unpack) $ T.words numbers'

  case findPairWithSum 2020 numbers of
    Nothing -> error "Can't find :'("
    Just (x, y) ->
      putStrLn $
        "Found! The two number is "
          <> show x
          <> " & "
          <> show y
          <> ", and their product is "
          <> show (x * y)

-- | Find a pair (two numbers) in a (sorted) list within a given range that
-- adds up to 'expected' by simply traversing simultaneously from both the
-- 'fromI' and 'toI' index towards middle to find a match until both
-- pointers meet, returns 'Nothing' if not found.
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

-- | Find a pair (two numbers) in a (sorted) list that adds up to 'expected' by
-- simply traversing from both ends of the list towards middle to find a match
-- until both pointers meet, returns 'Nothing' if not found
findPairWithSum ::
  -- | Expected sum
  Int ->
  -- | Target vector
  Vector Int ->
  Maybe (Int, Int)
findPairWithSum expected xs = findPairWithSumFromTo expected 0 (V.length xs - 1) xs
