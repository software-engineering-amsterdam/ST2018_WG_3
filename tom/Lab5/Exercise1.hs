module Lab51 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck hiding (forAll)
import Lecture5

{-- Assignment 1 (~5h) --}

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
            
exampleXF :: Grid
exampleXF = [[0,0,0,3,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [2,0,0,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]

solveAndShowX :: Grid -> IO[()]
solveAndShowX gr = solveShowNsX (initNodeX gr)

solveShowNsX :: [Node] -> IO[()]
solveShowNsX = sequence . fmap showNode . solveNsX

checkSolveX :: Grid -> Bool
checkSolveX gr = and $ map (consistent.fst) (solveNsX (initNodeX gr))

initNodeX :: Grid -> [Node]
initNodeX gr = let s = grid2sud gr in 
              if (not . consistentX) s then [] 
              else [(s, constraintsX s)]
              
emptyNX :: Node
emptyNX = (\ _ -> 0,constraintsX (\ _ -> 0))

bloXX :: [[Int]]
bloXX = [[2..4], [6..8]]

blX :: Int -> [Int]
blX x = concat $ filter (elem x) bloXX

subX :: Sudoku -> (Row,Column) -> [Value]
subX s (r,c) = 
  [ s (r',c') | r' <- blX r, c' <- blX c ]
              
consistentX :: Sudoku -> Bool
consistentX s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subXInjective s (r,c) |
                    r <- [2,6], c <- [2,6] ]

subXInjective :: Sudoku -> (Row,Column) -> Bool
subXInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subX s (r,c))
                    
constraintsX :: Sudoku -> [Constraint] 
constraintsX s = sortBy length3rd 
    [(r,c, freeAtPosX s (r,c)) | 
                        (r,c) <- openPositions s ]

freeInSubX :: Sudoku -> (Row,Column) -> [Value]
freeInSubX s (r,c) = freeInSeq (subX s (r,c))

freeAtPosX :: Sudoku -> (Row,Column) -> [Value]
freeAtPosX s (r,c) = 
  (freeInRow s r) 
   `intersect` (freeInColumn s c) 
   `intersect` (freeInSubgrid s (r,c)) 
   `intersect` (freeInSubX s (r,c)) 

pruneX :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
pruneX _ [] = []
pruneX (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneX (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneX (r,c,v) rest
  | sameblock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblockX (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneX (r,c,v) rest
  | otherwise = (x,y,zs) : pruneX (r,c,v) rest

sameblockX :: (Row,Column) -> (Row,Column) -> Bool
sameblockX (r,c) (x,y) = blX r == blX x && blX c == blX y 

extendNodeX :: Node -> Constraint -> [Node]
extendNodeX (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         pruneX (r,c,v) constraints) | v <- vs ]
         
succNodeX :: Node -> [Node]
succNodeX (s,[]) = []
succNodeX (s,p:ps) = extendNodeX (s,ps) p

solveNsX :: [Node] -> [Node]
solveNsX = search succNodeX solved 
