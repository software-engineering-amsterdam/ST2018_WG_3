module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck hiding (forAll)
import SetOrd

------ | Question 2 (2h) | ------

randomGen :: Int -> IO Int
randomGen n = getStdRandom (randomR (0,n))

generateSetInt :: IO (Set Int)
generateSetInt = do
  n <- randomGen 50
  p <- randomGen 6
  s1 <- if p < 6 then generateSetInt else return (Set [])
  return $ insertSet n s1


------ | Question 3 (2h) | ------

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set s1) (Set s2) = Set [ x | x <- s1, elem x s2 ]

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set s1) (Set s2) = Set [ x | x <- s1, not (elem x s2) ]


------ | Question 5 | ------

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos a = a

------ | Question 6 | ------

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos a = a