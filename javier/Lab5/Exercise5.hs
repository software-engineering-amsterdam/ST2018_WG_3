--------- Javier Bermejo Razquin -----------
module Exercise5

where

import Lecture5
import Exercise1
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck

--------- Exercise 5 45 minutes -----------
rsolveNsExtra :: [Node] -> IO [Node]
rsolveNsExtra ns = rsearch rsuccNodeExtra solved (return ns)

rsuccNodeExtra :: Node -> IO [Node]
rsuccNodeExtra (s,cs) = do
                          xs <- getRandomCnstr cs
                          if null xs then return []
                            else return (extendNodeExtra (s,cs\\xs) (head xs))

doExercise5 = do
              [r] <- rsolveNsExtra [emptyN]
              showNode r
              s  <- genProblem r
              showNode s
              solveShowNsExtra [s]
