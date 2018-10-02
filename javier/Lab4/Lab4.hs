------- Javier Bermejo Razquin -------
module Lab4

where

import Lecture4
import SetOrd
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck

---- Chapter 4 questions (1h15mins)-------
--Russell paradox?
--Example 4.25
--Exercise 4.43
--In general, i understood everything

--- Implement random data generator Set Int 2h ---
randomNumber :: Int -> IO Int
randomNumber n = getStdRandom (randomR (1,n))

generateSetInt :: IO (Set Int)
generateSetInt = do
  n <- randomNumber 10
  p <- randomNumber 10
  s1 <- if (p<5) then generateSetInt else return (insertSet n emptySet)
  s2 <- if (p<5) then generateSetInt else return (insertSet n emptySet)
  return (unionSet s1 s2)

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = do
    p <- choose (1,10) :: Gen Int
    x <- arbitrary
    xs <- if (p<8) then arbitrary else return emptySet
    return (insertSet x xs)

generateSetIntQC :: IO (Set Int)
generateSetIntQC = generate arbitrary


--- Implement operations for set intersection, union and difference 2h---
intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set [])     set2 = emptySet
intersectionSet (Set (x:xs)) set2 =
  if (inSet x set2)
    then insertSet x (intersectionSet (Set xs) set2)
    else intersectionSet (Set xs) set2

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet set1 (Set [])     = set1
differenceSet set1 (Set (x:xs)) =
  if (inSet x set1)
    then differenceSet (deleteSet x set1) (Set xs)
    else differenceSet set1 (Set xs)

doTestIntersection1Aux :: IO Bool
doTestIntersection1Aux = do
  set1 <- generateSetInt
  set2 <- generateSetInt
  return ((subSet (intersectionSet set1 set2) set1)
        && subSet (intersectionSet set1 set2) set2)

doTestIntersection1 :: Int -> IO String
doTestIntersection1 n = do
  result <- doTestIntersection1Aux
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestIntersection1 (n-1)

doTestIntersection2Aux :: IO Bool
doTestIntersection2Aux = do
  set1 <- generateSetIntQC
  set2 <- generateSetIntQC
  return ((subSet (intersectionSet set1 set2) set1)
        && subSet (intersectionSet set1 set2) set2)

doTestIntersection2 :: Int -> IO String
doTestIntersection2 n = do
  result <- doTestIntersection2Aux
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestIntersection2 (n-1)

doTestUnion1Aux :: IO Bool
doTestUnion1Aux = do
  set1 <- generateSetInt
  set2 <- generateSetInt
  return ((subSet set1 (unionSet set1 set2))
        && subSet set2 (unionSet set1 set2))

doTestUnion1 :: Int -> IO String
doTestUnion1 n = do
  result <- doTestUnion1Aux
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestUnion1 (n-1)

doTestUnion2Aux :: IO Bool
doTestUnion2Aux = do
  set1 <- generateSetIntQC
  set2 <- generateSetIntQC
  return ((subSet set1 (unionSet set1 set2))
        && subSet set2 (unionSet set1 set2))

doTestUnion2 :: Int -> IO String
doTestUnion2 n = do
  result <- doTestUnion2Aux
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestUnion2 (n-1)

set2List :: Ord a => Set a -> [a]
set2List (Set xs) = xs

doTestDifference1Aux :: IO Bool
doTestDifference1Aux = do
  set1 <- generateSetInt
  set2 <- generateSetInt
  return (foldr (&&) True
            (
              map
              (\x -> (inSet x set1) && (not(inSet x set2)))
              (set2List(differenceSet set1 set2))
            )
         )

doTestDifference1 :: Int -> IO String
doTestDifference1 n = do
  result <- doTestDifference1Aux
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestDifference1 (n-1)

doTestDifference2Aux :: IO Bool
doTestDifference2Aux = do
  set1 <- generateSetIntQC
  set2 <- generateSetIntQC
  return (foldr (&&) True
            (
              map
              (\x -> (inSet x set1) && (not(inSet x set2)))
              (set2List(differenceSet set1 set2))
            )
         )

doTestDifference2 :: Int -> IO String
doTestDifference2 n = do
  result <- doTestDifference2Aux
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestDifference1 (n-1)

------ Chapter 5 Questions 1h -------
--Pag 182 of the pdf , no Questions


----- Binary relations as a list of pairs 30 mins-----
----- Symetric closure -----
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (x:xs) = if (inList (swap x) xs)
  then symClos(xs)
  else [x,(swap x)] ++ symClos(xs)

inList x xs = elem x xs

----- Transitive closure of a binary relation 1h30mins-----
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos xs = keepSearching xs xs

keepSearching :: Ord a => Rel a -> Rel a -> Rel a
keepSearching rel1 rel2 = if (nub(rel1++(rel1@@rel2)) == nub(rel1++rel2))
  then nub(rel1++rel2)
  else keepSearching (rel1++(rel1@@rel2)) rel2


----- Testing both functions (symClos && trClos)  35mins + (10:00) -----
generateRel :: IO (Rel Int)
generateRel = do
 n1 <- randomNumber 10
 n2 <- randomNumber 10
 p <- randomNumber 10
 s1 <- return [(n1,n2)]
 s2 <- if (p<8) then generateRel else return []
 return (s1++s2)

removeSym :: Rel Int -> Rel Int
removeSym [] = []
removeSym (x:xs) = if ((inList x xs) || (inList (swap x) xs))
  then removeSym xs
  else [x] ++ (removeSym xs)

testSymClosure :: IO Bool
testSymClosure = do
  xs <- generateRel
  if (2*(length (removeSym (nub xs))) == (length (symClos (nub xs)))) then return True
    else return False

doTestSymClosure :: Int -> IO String
doTestSymClosure n = do
  result <- testSymClosure
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestSymClosure (n-1)
