module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck hiding (forAll)
import SetOrd

forAll = flip all

{-- Assignment 1 --}

{--

* The Russell paradox is not explained well. Is it because no universe is specified?
* []

--}


{-- Assignment 2 (1h) --}

-- From scratch random generator

pick :: Int -> IO Int
pick n = getStdRandom (randomR (0,n))

generateSetInt :: IO (Set Int)
generateSetInt = do
    h <- pick 100
    done <- pick 5
    t <- if done > 0 then generateSetInt else return (Set [])
    return $ insertSet h t


-- With QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        h <- arbitrary
        done <- choose (0,5) :: Gen Int
        t <- if done > 0 then arbitrary else return emptySet
        return $ insertSet h t
        

{-- Assignment 3 (2h) --}

-- unionSet is already defined

intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set s1) (Set s2) = Set [ x | x <- s1, x `elem` s2 ]

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set s1) (Set s2) = Set [ x | x <- s1, not (x `elem` s2) ]

-- Runs a test with n different inputs. (From lab 3)
runTest :: IO Bool -> Integer -> IO Bool
runTest _ 0 = return True
runTest f n = do
    result <- f
    recurse <- runTest f (n-1)
    return $ result && recurse


-- Test with from scratch

set2list :: Ord a => Set a -> [a]
set2list (Set xs) = xs

testDifferenceSet :: IO Bool
testDifferenceSet = do
    s1 <- generateSetInt
    s2 <- generateSetInt
    difference <- return $ differenceSet s1 s2
    result <- return $ forAll (set2list difference) (\x -> not (x `inSet` s2))
                    && forAll (set2list difference) (\x -> x `inSet` s1)
                    && difference `subSet` s1
    putStrLn $ "Test result " ++ (show result) ++ " for s1 " ++ (show s1) ++ ", s2 " ++ (show s2)
    return $ result
    
testIntersectSet :: IO Bool
testIntersectSet = do
    s1 <- generateSetInt
    s2 <- generateSetInt
    intersection <- return $ intersectSet s1 s2
    result <- return $ intersection `subSet` s1
                    && intersection `subSet` s2
    putStrLn $ "Test result " ++ (show result) ++ " for s1 " ++ (show s1) ++ ", s2 " ++ (show s2)
    return $ result

testUnionSet :: IO Bool
testUnionSet = do
    s1 <- generateSetInt
    s2 <- generateSetInt
    union <- return $ unionSet s1 s2
    result <- return $ s1 `subSet` union
                    && s2 `subSet` union
    putStrLn $ "Test result " ++ (show result) ++ " for s1 " ++ (show s1) ++ ", s2 " ++ (show s2)
    return $ result
    
-- Functions to run 100 instances of the tests
runTestDifferenceSet, runTestIntersectSet, runTestUnionSet :: IO Bool
runTestDifferenceSet = runTest testDifferenceSet 100
runTestIntersectSet = runTest testIntersectSet 100
runTestUnionSet = runTest testUnionSet 100


-- Test with QuickCheck

prop_differenceSet :: Ord a => Set a -> Set a -> Bool
prop_differenceSet s1 s2 = let difference = differenceSet s1 s2 in
                           forAll (set2list difference) (\x -> not (x `inSet` s2))
                        && forAll (set2list difference) (\x -> x `inSet` s1)
                        && difference `subSet` s1
                                   
prop_intersectSet :: Ord a => Set a -> Set a -> Bool
prop_intersectSet s1 s2 = let intersection = intersectSet s1 s2 in
                          intersection `subSet` s1
                       && intersection `subSet` s2
                       
prop_unionSet :: Ord a => Set a -> Set a -> Bool
prop_unionSet s1 s2 = let union = unionSet s1 s2 in
                      s1 `subSet` union
                   && s2 `subSet` union


{-- Assignment 5 (15m) --}

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = xs ++ [ (b, a) | (a, b) <- xs, not ((b, a) `elem` xs) ]


{-- Assignment 6 (15m) --}

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- repeatedLaw from Lab3
repeatedLaw :: Eq a => (a -> a) -> a -> a
repeatedLaw law f = let applied = law f in
    if law applied == applied
        then applied
        else repeatedLaw law applied

trClos :: Ord a => Rel a -> Rel a
trClos xs = repeatedLaw (\x -> nub (x ++ x @@ xs)) xs


{-- Assignment 7 --}

-- Test symClos
prop_symClos :: Ord a => Rel a -> Bool
prop_symClos r = let rsym = symClos r
                 in  forAll r (\x -> swap x `elem` rsym) -- Every element has an inverse
                  && forAll rsym (\x -> swap x `elem` r || x `elem` r) -- Every element in the rsym exists (inverted or normal) in the original r
                  
-- Test trClos
prop_trClos :: Ord a => Rel a -> Bool
prop_trClos r = undefined