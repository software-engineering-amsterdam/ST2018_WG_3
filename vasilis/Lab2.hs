
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)




--Assingment 1 (Time: 2h )


exc1 :: IO [Int]
exc1  = do
            xs <- probs 10000  -- <-  used in monads
            xs1 <- return [x | x <- xs, x > 0 && x < 0.25]
            xs2 <- return [x | x <- xs, x >= 0.25 && x < 0.5]
            xs3 <- return [x | x <- xs, x >= 0.5 && x < 0.75]
            xs4 <- return [x | x <- xs, x >= 0.75 && x < 1]
            return (map length [xs1, xs2, xs3, xs4])

            {- Generating 10000 numbers using given probs.
            Passing into 4 different lists, each one for every span (0,0.25]..etc
            returning a list with the lenght of every sublist and checking whether it's in wanted span.
            Results(1st time): [2530,2451,2496,2523]. (2nd): [2515,2511,2490,2484]. (3rd): [2544,2439,2526,2491]
            -}

--Assingment 2 (Time: 4h)
--Pending the test

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
           | ( (a + b) < c || (a + c) < b || (c + b) < a ) = NoTriangle -- checks if sum of two given sides exceed the third side, to be a triangle
           | ( (a * a) + (b * b) == (c * c) ) || ( (a * a) + (c * c) == (b * b) ) || ( (c * c) + (b * b) == (a * a) ) = Rectangular -- checks if the sum of two squared sides exceed the squared third
           | ( a == b && b == c ) = Equilateral -- checks if all sides are even
           | ( ( a == b ) || ( a == c ) || ( c == b ) ) = Isosceles --checks if two sides are even
           | otherwise = Other

--checking with quickcheck and passed all 100 test in each test except other triangle
testnotr a b c = ( (a + b) < c || (a + c) < b || (c + b) < a ) --> triangle a b c == NoTriangle
testrec a b c = ( a > 0 && b > 0 && c > 0 && ((a * a) + (b * b) == (c * c) ) || ( (a * a) + (c * c) == (b * b) ) || ( (c * c) + (b * b) == (a * a))) --> triangle a b c == Rectangular
testeq a b c = (a > 0 && b > 0 && c > 0 && a == b && b == c ) --> triangle a b c == Equilateral
testisc a b c = (a > 0 && b > 0 && c > 0 && (a /= b || b/= c)) && ( ( a == b ) || ( a == c ) || ( c == b ) ) --> triangle a b c == Isosceles
--testother a b c = (a > 0 && b > 0 && c > 0) --> triangle a b c == Other
-- I couldn't test the other tringle type
