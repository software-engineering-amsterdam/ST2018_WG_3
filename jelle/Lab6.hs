module Lab6 where

import Data.List
import System.Random
import Lecture6

-- Question 1 (45min)

{-|
  My implementation:
  
  exM :: Integer -> Integer -> Integer -> Integer
  exM x 1 n = rem x n
  exM x e n
    | odd e = (exM x (e - 1) n) * (rem x n)
    | otherwise = rem ((exM x (e `div` 2) n)^2) n
-}