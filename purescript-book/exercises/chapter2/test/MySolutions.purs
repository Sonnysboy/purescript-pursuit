module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Data.Number (pi)
import Data.Number (sqrt)

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * (r * r)

leftoverCents amt = amt `rem` 100