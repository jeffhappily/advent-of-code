# Discover

Automate the process of importing all modules from `src/` and running the `main` functions that they export. Source code is mainly from [sydtest-discover](https://github.com/NorfairKing/sydtest/blob/5b0eee208753e3554d9b158a6e48b1760514aed0/sydtest-discover).

Instead of 
```hs
module Main (main) where

import Day1 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified
import Prelude

main :: IO ()
main = do
  Day1.main
  Day2.main
  Day3.main
  Day4.main
```

just do
```hs
{-# OPTIONS_GHC -F -pgmF main-discover #-}
```
