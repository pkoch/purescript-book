module Test.MySolutions where

import Prelude

import Data.Array (filter, length)
import Math (pow)
import Test.Examples (factors)

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

isPrime :: Int -> Boolean
isPrime 0 = false
isPrime 1 = false
isPrime n = length (factors n)  == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  aa <- a
  bb <- b
  [ [aa, bb] ]
