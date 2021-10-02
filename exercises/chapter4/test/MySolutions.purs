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
squared = map (_ `pow` 2.0)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 5 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite l = (_ >= 0.0) <$?> l
