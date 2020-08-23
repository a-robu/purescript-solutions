module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, head, tail, (..))
import Data.Foldable (length)
import Data.Maybe (fromMaybe)
import Test.Examples (factorsV3)

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

isPrime :: Int -> Boolean
isPrime n = length (factorsV3 n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct first second = do
  a <- first
  b <- second
  pure [ a, b ]

triples :: Int -> Array (Array Int)
triples n = do
    a <- 1 .. n
    b <- a .. n
    c <- 1 .. n
    guard $ a * a + b * b == c * c
    pure [ a, b, c ]
