module Test.MySolutions where

import Prelude

import Global (readFloat)
import Math (sqrt, pow, pi, e)

-- import Test.NoPeeking.Solutions (circleArea)

diagonal :: Number -> Number -> Number
diagonal b h = sqrt (pow b 2.0 + pow h 2.0)

circleArea :: Number -> Number
circleArea r = pi * pow r 2.0

addE :: String -> Number
addE s = readFloat s +  e
