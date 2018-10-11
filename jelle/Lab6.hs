module Lab6 where

import Data.List
import System.Random
import Lecture6

-- Question 1 (1h)

{-|
  My implementation:
  
  exM _ 0 n = rem 1 n
  exM x 1 n = rem x n
  exM x e n
    | odd e = (exM x (e - 1) n) * rem x n
    | otherwise = rem ((exM x (e `div` 2) n)^2) n
-}

exM' :: Integer -> Integer -> Integer -> Integer
exM' = expM -- to be replaced by a fast version