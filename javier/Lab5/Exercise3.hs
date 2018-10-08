--------- Javier Bermejo Razquin -----------
module Exercise3

where

import Lecture5
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck


--------- Exercise 3 2:50---------
uniqueSolution :: Grid -> Bool
uniqueSolution s =
  if (isLongerThan 1 (solveNs(initNode s))) then
    False
  else
    --True
    deleteHints 1 (solveNs(initNode s))

isLongerThan :: Int -> [Node] -> Bool
isLongerThan n xs = not (null (drop n xs))

test s = map (0*) s(1,1)

{--deleteHints :: Int -> Sudoku -> (Int,Int)-> [Sudoku]
deleteHints n s (r,c) =
  if (n>0) then
    [s(r,c) = 0] ++
    (deleteHints (n-1) s (r+1,c)) ++
    (deleteHints (n-1) s (r,c+1)) ++
    (deleteHints (n-1) s (r+1,c+1)) ++
  else
    xs--}
