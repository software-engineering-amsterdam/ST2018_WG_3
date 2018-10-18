module Lab6 where

import Data.List
import System.Random
import Lecture6
import Test.QuickCheck
import System.TimeIt

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Question 1 (2h)

{-|
  My implementation:
  
  exM _ 0 n = rem 1 n
  exM x 1 n = rem x n
  exM x e n
    | odd e = rem ((exM x (e - 1) n) * rem x n) n
    | otherwise = rem ((exM x (e `div` 2) n)^2) n
-}

testExM :: Integer -> Integer -> Integer -> Bool
testExM x e n = n > 0 && e >= 0 --> exM x e n == expM x e n

{-|
  *Lab6> quickCheck testExM
  +++ OK, passed 100 tests
-}

-- Question 2 (2h)

newTest :: IO(Integer)
newTest = do
  return $ exM 5162 7208246 785

oldTest :: IO(Integer)
oldTest = do
  return $ expM 5162 7208246 785

{-|
  I used :set +s in the ghci environment.

  *Lab6> newTest
  344
  (0.10 secs, 80,496 bytes)
  
  *Lab6> oldTest
  344
  (0.83 secs, 31,163,672 bytes)

  This example clearly shows the improvement in 
  time and memory usage.
-}

-- Question 3 ()
