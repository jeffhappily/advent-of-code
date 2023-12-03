{-# LANGUAGE LambdaCase #-}

module Year2020.Day3.Part1 (main, solve, mkRow) where

import Data.Text (Text)
import Data.Text qualified as T

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

-- | Get element from row using column number, row would behave like
-- an infinite repeating list, so overflow `i` would start over from
-- the begining of the list
getCol :: Row -> Int -> Space
getCol (Row r) i = r !! (i `mod` length r)

-- | Create row from input string
mkRow :: Text -> Row
mkRow xs = maybe (Row []) (Row . reverse) xs'
  where
    spaceMap acc c = case (c, acc) of
      (_, Nothing) -> Nothing
      ('.', Just xs') -> Just $ OpenSquare : xs'
      ('#', Just xs') -> Just $ Tree : xs'
      _ -> Nothing
    xs' = T.foldl' spaceMap (Just []) xs

-- | Get every `n` element from the list
--
-- >>> every 2 [1,2,3,4,5,6,7]
-- [1,3,5,7]
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

main :: Text -> IO ()
main content = do
  let rows = mkRow <$> T.lines content

  print $ solve rows 3 1
