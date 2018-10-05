--------- Javier Bermejo Razquin -----------
module Exercise1

where

import Lecture5
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck

--------- Exercise 1 8h -------------
solveAndShowExtra :: Grid -> IO[()]
solveAndShowExtra gr = solveShowNsExtra (initNodeExtra gr)

solveShowNsExtra :: [Node] -> IO[()]
solveShowNsExtra = sequence . fmap showNode . solveNsExtra

solveNsExtra :: [Node] -> [Node]
solveNsExtra = search succNodeExtra solved

succNodeExtra :: Node -> [Node]
succNodeExtra (s,[]) = []
succNodeExtra (s,p:ps) = extendNodeExtra (s,ps) p

extendNodeExtra :: Node -> Constraint -> [Node]
extendNodeExtra (s,constraintsExtra) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         pruneExtra (r,c,v) constraintsExtra) | v <- vs ]

pruneExtra :: (Row,Column,Value)
       -> [Constraint] -> [Constraint]
pruneExtra _ [] = []
pruneExtra (r,c,v) ((x,y,zs):rest)
   | r == x = (x,y,zs\\[v]) : pruneExtra (r,c,v) rest
   | c == y = (x,y,zs\\[v]) : pruneExtra (r,c,v) rest
   | sameblock (r,c) (x,y) =
         (x,y,zs\\[v]) : pruneExtra (r,c,v) rest
   | sameblockExtra (r,c) (x,y) =
         (x,y,zs\\[v]) : pruneExtra (r,c,v) rest
   | otherwise = (x,y,zs) : pruneExtra (r,c,v) rest

sameblockExtra :: (Row,Column) -> (Row,Column) -> Bool
sameblockExtra (r,c) (x,y) = blE r == blE x && blE c == blE y


constraintsExtra :: Sudoku -> [Constraint]
constraintsExtra s = sortBy length3rd
   [(r,c, freeAtPosExtra s (r,c)) |
                           (r,c) <- openPositions s ]

freeAtPosExtra :: Sudoku -> (Row,Column) -> [Value]
freeAtPosExtra s (r,c) =
 (freeInRow s r)
   `intersect` (freeInColumn s c)
   `intersect` (freeInSubgrid s (r,c))
   `intersect` (freeInExtraSubgrid s (r,c))

freeInExtraSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInExtraSubgrid s (r,c)
 | ((subGridExtra s (r,c)) == []) = [1..9]
 | otherwise                      = freeInSeq (subGridExtra s (r,c))

subGridExtra :: Sudoku -> (Row,Column) -> [Value]
subGridExtra s (r,c) =
  [ s (r',c') | r' <- blE r, c' <- blE c ]

blE :: Int -> [Int]
blE x = concat $ filter (elem x) blocksExtra

blocksExtra :: [[Int]]
blocksExtra = [[2..4],[6..8]]

emptyNExtra :: Node
emptyNExtra = (\ _ -> 0,constraintsExtra (\ _ -> 0))

initNodeExtra :: Grid -> [Node]
initNodeExtra gr = let s = grid2sud gr in
              if (not . consistentExtra) s then []
              else [(s, constraintsExtra s)]

consistentExtra :: Sudoku -> Bool
consistentExtra s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridExtraInjective s (r,c) |
                    r <- [2,6], c <- [2,6]]

subgridExtraInjective :: Sudoku -> (Row,Column) -> Bool
subgridExtraInjective s (r,c) = injective vs where
   vs = filter (/= 0) (subGrid s (r,c))

exampleX :: Grid
exampleX = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

exampleXX :: Grid
exampleXX = [[0,0,0,0,9,0,0,0,1],
            [0,6,8,0,0,3,0,0,9],
            [0,0,2,0,7,1,8,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,9,0,0,0,0,6,0],
            [5,0,0,0,3,0,0,0,0],
            [0,7,0,0,0,0,0,5,0],
            [0,0,0,0,0,4,7,0,0],
            [4,0,3,0,0,0,9,0,0]]
