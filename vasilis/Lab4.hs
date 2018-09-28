module Lab4 
where
 
import Data.List
import System.Random
import Test.QuickCheck  

--Assingment 5 (1.30h)--

type Rel a = [(a,a)]

{- maps conn to list of tuples a and added to the given list of tuples
conn inverts each tuple. (1,2) to (2,1)
nub removes the duplicates example given list [(1,2),(3,3)] turned into [(1,2),(2,1),(3,3)] and not to [(1,2),(2,1),(3,3),(3,3)]
-}
symClos :: Ord a => Rel a -> Rel a
symClos a =nub (a ++ map (conn) a) 
     where conn (first, second) = (second, first)
     
   
  {-
--Assingment 1--
--numGen :: Int -> IO Int
numGen :: IO Int
numGen = getStdRandom (randomR (1,100))


genSets 
-}

--genSets 0 = show xs
{-genSets n =
  do
    x <- numGen
    ns <- ns : x
	--genSets (n-1)
	if (n == 0) then return ns else genSets(n-1)
-}
{-genSets :: Int -> Int -> [Int]
genSets n x 
   | n == 0   = show x
   | otherwise = x : numGen (n-1)
   -}

-- Assingment 5--
{-
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a 
  -} 
   
   
   
   
   
   
   
