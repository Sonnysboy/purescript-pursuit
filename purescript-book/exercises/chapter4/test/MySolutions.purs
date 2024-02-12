module Test.MySolutions
where

import Prelude

import ChapterExamples (Person)
import Data.Array ((:))
import Data.Array.NonEmpty (length)
import Data.Picture 

factorial :: Int -> Int
factorial n 
  | n == 0 = 1
  | n == 1 = 1
  | n == 2 = 2
  | n == 3 = 6
  | n == 4 = 24
  | n == 5 = 120
  | n == 6 = 720
  | n == 7 = 5040
  | n == 8 = 40320
  | otherwise = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
  | otherwise = factorial n / (factorial k * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k
  = pascal (n-1) k + pascal (n - 1) (k - 1)


sameCity :: Person -> Person -> Boolean
sameCity { address: { city : city1 } } { address: { city : city2 } } = (==) city2 city1

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton d _   = d

circleAtOrigin :: Shape
circleAtOrigin = Circle {x : 0.0, y : 0.0 } 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ size) = Circle origin (2.0 * size) 
doubleScaleAndCenter (Rectangle _ length width) = Rectangle origin (2.0 * length) (2.0 * width)
doubleScaleAndCenter (Line {x: startX, y: startY} { x: endX, y: endY }) = Line {x : startX * 2.0, y: startY * 2.0} { x: endX * 2.0, y: endY * 2.0}
doubleScaleAndCenter (Text pt text) = Text pt text
    where
        origin = { x : 0.0, y : 0.0 }