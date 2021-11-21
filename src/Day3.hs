{-# LANGUAGE LambdaCase #-}

module Day3 (main) where

import Prelude

data Space
  = OpenSquare
  | Tree
  deriving (Show)

-- | Check if current space is Tree
isTree :: Space -> Bool
isTree = \case
  OpenSquare -> False
  Tree -> True

newtype Row = Row {getRow :: [Space]}
  deriving (Show)

{- | Get element from row using column number, row would behave like
 an infinite repeating list, so overflow `i` would start over from
 the begining of the list
-}
getCol :: Row -> Int -> Space
getCol (Row r) i = r !! (i `mod` length r)

-- | Create row from input string
mkRow :: [Char] -> Row
mkRow xs = maybe (Row []) Row xs'
 where
  spaceMap = \case
    '.' -> Just OpenSquare
    '#' -> Just Tree
    _ -> Nothing
  xs' = traverse spaceMap xs

{- | Get every `n` element from the list

 >>> every 2 [1,2,3,4,5,6,7]
 [1,3,5,7]
-}
every :: Int -> [a] -> [a]
every n = \case
  [] -> []
  (x : xs) -> x : every n (drop (n - 1) xs)

solve ::
  -- | List of input role
  [Row] ->
  -- | Right
  Int ->
  -- | Down
  Int ->
  -- | Number of tree encountered
  Int
solve xs r d =
  length
    . filter isTree
    . fmap (uncurry getCol)
    . every d
    . zip xs
    $ enumFromThen 0 r

main :: IO ()
main = do
  content <- readFile "input/d3.txt"

  let rows = mkRow <$> lines content
  let inputs =
        [ (1, 1)
        , (3, 1)
        , (5, 1)
        , (7, 1)
        , (1, 2)
        ]

  putStrLn "Part 1"
  print $ solve rows 3 1

  putStrLn "Part 2"
  print $
    product . fmap (uncurry $ solve rows) $ inputs
