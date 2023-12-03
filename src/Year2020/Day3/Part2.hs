{-# LANGUAGE LambdaCase #-}

module Year2020.Day3.Part2 (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Year2020.Day3.Part1 (mkRow, solve)

main :: Text -> IO ()
main content = do
  let rows = mkRow <$> T.lines content
  let inputs =
        [ (1, 1),
          (3, 1),
          (5, 1),
          (7, 1),
          (1, 2)
        ]

  print $
    product . fmap (uncurry $ solve rows) $
      inputs
