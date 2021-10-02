module Test.MySolutions where

import Prelude

import Data.Array (filter, length)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

countEven :: Array Int -> Int
countEven = filter isEven >>> length
