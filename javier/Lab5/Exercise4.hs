--------- Javier Bermejo Razquin -----------
module Exercise4

where

import Lecture5
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck

--------- Exercise 4 4h -----------
doExercise4 :: IO()
doExercise4 = do [r] <- rsolveNs [emptyN]
              showNode r
              s <- genProblemEmptyBlocks 20 r
              showNode s

genProblemEmptyBlocks :: Int -> Node -> IO Node
genProblemEmptyBlocks n r = do ys <- randomize xs
                               return (minimalizeBlocks n r ys)
  where xs = filledPositions (fst r)

minimalizeBlocks :: Int -> Node -> [(Row,Column)] -> Node
minimalizeBlocks 0  n _  = n
minimalizeBlocks nb n [] = n
minimalizeBlocks nb n ((r,c):rcs) | uniqueSol n' = minimalizeBlocks (nb-1) n' rcs
                                  | otherwise    = minimalizeBlocks nb n  rcs
  where n' = eraseBlock n (r,c)

eraseBlock :: Node -> (Row,Column) -> Node
eraseBlock n (r,c) = (s, constraints s)
  where s = eraseSudokuBlock (fst n) (combinations (bl r) (bl c))

combinations :: [Int] -> [Int] -> [(Int,Int)]
combinations xs ys = [(x,y) | x <- xs, y <- ys]

eraseSudokuBlock :: Sudoku -> [(Int,Int)] -> Sudoku
eraseSudokuBlock s [] = s
eraseSudokuBlock s (x:xs) = eraseSudokuBlock (eraseS s x) xs

--- Normally 3 boxes removed
--- Sometimes 4 boxes are removed, but no more
