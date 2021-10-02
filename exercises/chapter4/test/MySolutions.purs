module Test.MySolutions where

import Prelude

import Data.Array (filter, length)
import Math (pow)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

countEven :: Array Int -> Int
countEven = filter isEven >>> length

squared :: Array Number -> Array Number
squared = map (\x -> pow x 2.0)
