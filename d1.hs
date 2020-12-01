import Data.List
import Data.Vector as V

main :: IO ()
main = do
  numbers' <- readFile "d1-input.txt"
  let numbers = V.fromList . sort . fmap (\x -> read x :: Int) $ words numbers'

  case part1 numbers of
    Nothing -> putStrLn "Can't find :'("
    Just (x, y) -> putStrLn $ "Found! The two number is " <> show x <> " & " <> show y <> ", and their product is " <> show (x * y)

  case part2 numbers of
    Nothing -> putStrLn "Can't find :'("
    Just (x, y, z) -> putStrLn $ "Found! The three number is " <> show x <> " & " <> show y <>" & " <> show z <>  ", and their product is " <> show (x * y * z)


-- | Find a pair in a (sorted) list within a given range that adds up to 'expected'
-- by simply traversing simultaneously from both the given from and to index until
-- both pointers meet, returns 'Nothing' if not found.
findPairWithSumFromTo :: Int        -- ^ Expected sum
                      -> Int        -- ^ From index
                      -> Int        -- ^ To index
                      -> Vector Int -- ^ Target vector
                      -> Maybe (Int, Int)
findPairWithSumFromTo expected fromI toI xs = go fromI toI
  where
    go startI endI
      -- Avoid index out of bounds
      | startI >= V.length xs || endI >= V.length xs
        = Nothing
      | startI == endI = Nothing
      | sum < expected = go (startI + 1) endI
      | sum > expected = go startI (endI - 1)
      | otherwise = Just (start, end)
      where
        start = getVal startI
        end = getVal endI
        sum = start + end
    getVal :: Int -> Int
    getVal = (V.!) xs

-- | Find a pair in a (sorted) list that adds up to 'expected' by simply
-- traversing from both ends of the list until both pointers meet, returns
-- 'Nothing' if not found
findPairWithSum :: Int -> Vector Int -> Maybe (Int, Int)
findPairWithSum expected xs = findPairWithSumFromTo expected 0 (V.length xs - 1) xs

-- | Find a pair (two numbers) in a list that adds up to 2020
part1 :: Vector Int -> Maybe (Int, Int)
part1 = findPairWithSum 2020

-- | Find a triplet (three numbers) in a list that adds up to 2020, by looping
-- through the list, for every number @a@, find two numbers that adds up to
-- @2020 - a@
part2 :: Vector Int -> Maybe (Int, Int, Int)
part2 xs = V.ifoldr 
            (\i a b -> case b of 
              Nothing -> case findPairWithSumFromTo (2020 - a) 0 (i - 1) xs of
                Just (y, z) -> Just (a, y, z)
                Nothing -> Nothing
              Just c -> Just c)
            Nothing
            xs
