module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck hiding (forAll)
import SetOrd

forAll = flip all


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

instance (Ord a, Arbitrary a, Num a) => Arbitrary (Set a) where
    arbitrary = do
        (Positive h) <- arbitrary :: (Ord a, Arbitrary a, Num a) => Gen (Positive a)
        done <- choose (0,5) :: Gen Int
        t <- if done > 0 then arbitrary else return emptySet
        return $ insertSet h t
        
generateSetIntQC :: IO (Set Int)
generateSetIntQC = generate arbitrary


{-- Assignment 3 --}

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


{-- Assignment 5 (1.30h) --}

----- Symetric closure -----
type Rel a = [(a,a)]
{- rev reverses the elements of a tuple (ex. rev (1,2) = (2,1)). 
nub removes duplicates (used from lab's html)
maps every tuple of the list a with the rev function and adds it on the list of tuples, a
-}
symClos :: Ord a => Rel a -> Rel a
symClos a = nub (a ++ map (rev) a) 
     where rev (first, second) = (second, first)
     
     
{-- Assignment 6 --}
     
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


{-- Assignment 7 --}

-- Whether xs is a sublist of ys (From Lab3)
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] ys = True
isSublist xs [] = False
isSublist (x:xs) ys | x `elem` ys = isSublist xs (delete x ys)
                    | otherwise   = False


-- Test symClos
prop_symClos :: Ord a => Rel a -> Bool
prop_symClos r = let rsym = symClos r
                 in  forAll r (\x -> swap x `elem` rsym) -- Every element has an inverse
                  && forAll rsym (\x -> swap x `elem` r || x `elem` r) -- Every element in the rsym exists (inverted or normal) in the original r

-- Test trClos
prop_trClos :: Ord a => Rel a -> Bool
prop_trClos r = let rclos = trClos r
                    transitiveChecks :: [Bool]
                    transitiveChecks = [ ((a, c) `elem` rclos) | (a, b) <- rclos, (c, d) <- rclos, b == c ]
                in  (nub r) `isSublist` (nub rclos) -- Check if r is subset of rclos
                 && foldr (&&) True transitiveChecks -- Check transitive property of rclos

-- Custom testing method

generateRel :: IO (Rel Int)
generateRel = do
    a <- pick 100
    b <- pick 100
    h <- return $ (a, b)
    done <- pick 5
    t <- if done > 0 then generateRel else return (h : [])
    return (h : t)

testSymClos :: IO Bool
testSymClos = do
    r <- generateRel
    result <- return $ prop_symClos r
    putStrLn $ "Test result " ++ (show result) ++ " for r = " ++ (show r)
    return result

testTrClos :: IO Bool
testTrClos = do
    r <- generateRel
    result <- return $ prop_trClos r
    putStrLn $ "Test result " ++ (show result) ++ " for r = " ++ (show r)
    return result

runTestSymClos, runTestTrClos :: IO Bool
runTestSymClos = runTest testSymClos 100
runTestTrClos = runTest testTrClos 100


-- With QuickCheck
qcSymClos = quickCheck (prop_symClos :: Rel Int -> Bool)
qcTrClos = quickCheck (prop_trClos :: Rel Int -> Bool)


{-- Assignment 8 --}

-- We can test this with QuickCheck

testDiffTCvTCSC :: Rel Int -> Bool
testDiffTCvTCSC r = trClos (symClos r) == symClos (trClos r)

quickCheckDiffTCvTCSC :: IO ()
quickCheckDiffTCvTCSC = quickCheck testDiffTCvTCSC


{--
Falsifiable with input [(0,1)].
The transitive closure of the symmetric closure of r is not equal to the symmetric closure
of the transitive closure of r. This is because first taking the symmetric closure creates
cyclic relations in the graph, which after taking the transitive closure result in
reflexivity, something that does not happen when you take the symmetric closure after the
transitive closure.
--}
