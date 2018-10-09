module Exercise3 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck hiding (forAll)
import Lecture5


{-- Assignment 3 (~2h) --}

{--
By testing the examples in Lecture5.hs with this implementation
of minimal, we obtain the following results:
Example 1: Not minimal
Example 2: Not minimal
Example 3: Minimal
Example 4: Not minimal
Example 5: ?
--}

minimal :: Grid -> Bool
minimal g = solutionCount g == 1
            && all (\x -> solutionCount x > 1) (smallerProblems g)

solutionCount :: Grid -> Int
solutionCount g = length (solveNs (initNode g))

smallerProblems :: Grid -> [Grid]
smallerProblems g =
    filter (/= g) [ replaceNested (r,c) 0 g | r <- idxPositions, c <- idxPositions ]
  where idxPositions = [0..8]

replaceNested :: (Int, Int) -> a -> [[a]] -> [[a]]
replaceNested (r,c) a g = replace r (replace c a (g !! r)) g

replace :: Int -> a -> [a] -> [a]
replace i a xs = take i xs ++ [a] ++ drop (i + 1) xs