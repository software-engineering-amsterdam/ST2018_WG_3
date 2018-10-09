module Exercise1 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck hiding (forAll)
import System.TimeIt
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
             [2,0,7,0,0,0,0,0,8],
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


-- Testing performance: (for assignment 2)

genProblemX :: Node -> IO Node
genProblemX n = do ys <- randomize xs
                   return (minimalizeX n ys)
   where xs = filledPositions (fst n)
   
uniqueSolX :: Node -> Bool
uniqueSolX node = singleton (solveNsX [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False
   
minimalizeX :: Node -> [(Row,Column)] -> Node
minimalizeX n [] = n
minimalizeX n ((r,c):rcs) | uniqueSolX n' = minimalizeX n' rcs
                          | otherwise     = minimalizeX n  rcs
  where n' = eraseNX n (r,c)

eraseNX :: Node -> (Row,Column) -> Node
eraseNX n (r,c) = (s, constraintsX s) 
  where s = eraseS (fst n) (r,c) 

randomProblemX :: IO Node
randomProblemX = do [r] <- rsolveNsX [emptyNX]
                    genProblemX r
                    
rsolveNsX :: [Node] -> IO [Node]
rsolveNsX ns = rsearch rsuccNodeX solved (return ns)

rsuccNodeX :: Node -> IO [Node]
rsuccNodeX (s,cs) = do xs <- getRandomCnstr cs
                       if null xs 
                         then return []
                         else return 
                           (extendNodeX (s,cs\\xs) (head xs))


perfTest :: IO Double
perfTest = do
    ts <- perfTest' 30
    return $ average ts

perfTest' :: Int -> IO [Double]
perfTest' n = do
    t <- perfTestSingle
    ts <- if n > 0 then perfTest' (n-1) else return []
    return (t : ts)

perfTestSingle :: IO Double
perfTestSingle = do
    r <- randomProblemX
    showNode r
    (t, _) <- timeItT (solveShowNsX [r])
    putStrLn (show t)
    return t

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)