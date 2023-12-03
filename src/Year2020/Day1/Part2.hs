module Year2020.Day1.Part2 (main) where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Year2020.Day1.Part1 (findPairWithSumFromTo)

main :: Text -> IO ()
main numbers' = do
  let numbers = V.fromList . sort . fmap (read . T.unpack) $ T.words numbers'
      result =
        V.ifoldr
          ( \i a b -> case b of
              Nothing ->
                (\(y, z) -> (a, y, z))
                  <$> findPairWithSumFromTo (2020 - a) 0 (i - 1) numbers
              Just c -> Just c
          )
          Nothing
          numbers

  case result of
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
