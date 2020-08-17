module Test.MySolutions where

import Prelude

import Data.Array (filter, head, tail)
import Data.Foldable (length)
import Data.Maybe (fromMaybe)

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not $ isEven $ n - 1

boolToInt :: Boolean -> Int
boolToInt false = 0
boolToInt true = 1

countEven :: Array Int -> Int
countEven [] = 0
countEven arr =
    let
        this = boolToInt $ isEven $ fromMaybe 0 (head arr)
        others = countEven $ fromMaybe [] (tail arr)
    in 
        this + others

-- Same as countEven, but implemented with existing functions.
trivialCountEven :: Array Int -> Int
trivialCountEven arr = length $ filter isEven arr

squared :: Array Number -> Array Number
squared = map \x -> x * x

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter \x -> x >= 0.0

infixl 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\x -> x >= 0.0) <$?> arr
