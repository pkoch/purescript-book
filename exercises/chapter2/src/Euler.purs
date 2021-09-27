module Euler where

import Prelude

import Data.Foldable (sum)
import Data.List (range, filter)

answer :: Int -> Int
answer n = range 0 (n - 1)
           # filter (\a -> mod a 3 == 0 || mod a 5 == 0)
           # sum
