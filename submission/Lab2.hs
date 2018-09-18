module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Control.Monad
import Control.Monad.Loops (whileM)

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)
           
-- From the lecture

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
  x <-  getRandomInt k
  y <- randomFlip x
  xs <- getIntL k (n-1)
  return (y:xs)

genIntList :: IO [Int]
genIntList = do
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

randomFlip :: Int -> IO Int
randomFlip x = do
   b <- getRandomInt 1
   if b==0 then return x else return (-x)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomInt' n = getStdRandom (randomR (-n,n))

testR :: Int -> Int -> ([Int] -> [Int])
                   -> ([Int] -> [Int] -> Bool) -> IO ()
testR k n f r = if k == n then print (show n ++ " tests passed")
               else do
                 xs <- genIntList
                 print ("doing " ++ show xs)
                 if r xs (f xs) then
                   do print ("pass on: " ++ show xs)
                      testR (k+1) n f r
                 else error ("failed test on: " ++ show xs)


{---------- Assignment 1 ----------}

testProbs :: Int -> IO [Int]
testProbs n = do
    results <- probs n
    fstQ <- return $ filter (\x -> x >= 0 && x < 0.25) results
    sndQ <- return $ filter (\x -> x >= 0.25 && x < 0.5) results
    thdQ <- return $ filter (\x -> x >= 0.5 && x < 0.75) results
    fthQ <- return $ filter (\x -> x >= 0.75 && x < 1) results
    return $ map length [fstQ, sndQ, thdQ, fthQ]
    
{-|
  Execution of the method:
  *Lab2> testProbs 10000

  Result:
  [2534,2439,2524,2503]

  The result varies each execution, but it is always a list of four numbers that are all around 1/4 of n. 
  Each of these number represent a quartile of the numbers of to n.
  The higher the n value gets, the closer the values of the quartiles (percentually) are to the 1/4 point.
-}


{---------- Assignment 2 ----------}

isTriangle, isEquilateral, isRectangular, isIsosceles :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a+b>=c && a+c>=b && b+c>=a     -- 
isEquilateral a b c = a == b && b == c            -- The triangle is equilateral if all sides are the same length.
isRectangular a b c = a*a+b*b==c*c || b*b+c*c==a*a || a*a+c*c==b*b  -- The triangle is rectangular if Pythagoras' theorem holds.
isIsosceles a b c = a == b || b == c || a == c  -- The triangle is isosceles if at least two sides are the same length.
-- The triangle is normal if it is a triangle but none of the other special properties hold.
isNormal a b c = isTriangle a b c && not (isEquilateral a b c) && not (isRectangular a b c) && not (isIsosceles a b c)

-- Tries some properties and returns the right Shape.
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | not $ isTriangle a b c = NoTriangle
    | isEquilateral a b c    = Equilateral
    | isRectangular a b c    = Rectangular
    | isIsosceles a b c      = Isosceles
    | otherwise              = Other

-- Testing

edgeGen :: IO Integer
edgeGen = getStdRandom (randomR (1,10))
pickGen :: Integer -> IO Integer
pickGen n = getStdRandom (randomR (0,n))
genInvalidTriangle, genEquilateral, genRectangular, genIsosceles :: IO (Integer, Integer, Integer)

-- Generates shapes that should be NoTriangle (because one edge has length 0)
genInvalidTriangle = do
    edge <- pickGen 2
    edge1 <- edgeGen
    edge2 <- edgeGen
    return $ case edge of 0 -> (edge1, edge2, edge1 + edge2 + 1)
                          1 -> (edge1, edge1 + edge2 + 1, edge2)
                          2 -> (edge1 + edge2 + 1, edge1, edge2)
                          otherwise -> undefined

-- Generate equilateral shapes (all edges of same length)
genEquilateral = do
    edge <- edgeGen
    return $ (edge, edge, edge)

-- Generate rectangular triangles (a^2+b^2=c^2 holds)
genRectangular = do
    edge <- pickGen 5
    n <- edgeGen -- Using Euclid's algorithm to generate Pythagorean triples.
    m_t <- edgeGen
    m <- return $ m_t + n -- Make sure m > n
    edge1 <- return $ m^2 - n^2
    edge2 <- return $ 2 * m * n
    edge3 <- return $ m^2 + n^2
    return $ case edge of 0 -> (edge1, edge2, edge3)
                          1 -> (edge2, edge3, edge1)
                          2 -> (edge3, edge1, edge2)
                          3 -> (edge1, edge3, edge2)
                          4 -> (edge2, edge1, edge3)
                          5 -> (edge3, edge2, edge1)
                          otherwise -> undefined    

-- Generates isosceles shapes (two edges of same length)
genIsosceles = do
    edge <- pickGen 2
    edge12 <- edgeGen
    edge3 <- edgeGen
    return $ case edge of 0 -> (edge12, edge12, edge3)
                          1 -> (edge12, edge3, edge12)
                          2 -> (edge3, edge12, edge12)
                          otherwise -> undefined

testTriangle :: (IO (Integer, Integer, Integer)) -> (Integer -> Integer -> Integer -> Bool) -> IO Bool
testTriangle f f' = do
    (x, y, z) <- f
    return $ f' x y z

runTestTriangle :: IO Bool -> Integer -> IO Bool
runTestTriangle _ 0 = return True
runTestTriangle f n = do
    result <- f
    recurse <- runTestTriangle f (n-1)
    return $ result && recurse

runTestInvalidTriangle, runTestEquilateral, runTestRectangular, runTestIsosceles :: Integer -> IO Bool
runTestInvalidTriangle = runTestTriangle $ testTriangle genInvalidTriangle (\a b c -> not (isTriangle a b c))
runTestEquilateral = runTestTriangle $ testTriangle genEquilateral isEquilateral
runTestRectangular = runTestTriangle $ testTriangle genRectangular isRectangular
runTestIsosceles = runTestTriangle $ testTriangle genIsosceles isIsosceles

runTriangleTests n = do
    a <- runTestInvalidTriangle n
    b <- runTestEquilateral n
    c <- runTestRectangular n
    d <- runTestIsosceles n
    return $ a && b && c && d
    
{-|
  Execution:
  *Lab2> runTriangleTests 10000

  Result:
  True

  The execution will generate 10000 tests for each type of triangle, so it will perform 40000 tests in total.
  Every exection of the runTriangleTests returns true, thus all triangle properties are valid.
-}


{---------- Assignment 3 ----------}

-- Testing properties strength 1h30

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

firstExercise :: Int -> Bool
firstExercise x = (even x && x > 3)

secondExercise :: Int -> Bool
secondExercise x = (even x || x > 3)

thirdExercise :: Int -> Bool
thirdExercise x = (firstExercise x || even x)

quicksortStronger :: (Num a1, Enum a1) => [(a2, a1 -> Bool)] -> [(a2, a1 -> Bool)]
quicksortStronger [] = []
quicksortStronger (x:xs) =
   quicksortStronger [ p | p <- xs, stronger [-10..10] (snd p) (snd x)]
   ++ [x]
   ++ quicksortStronger [ q | q <- xs, weaker [-10..10] (snd q) (snd x)]

printQuicksortStronger :: [String]
printQuicksortStronger =
    map fst $ quicksortStronger [("first",firstExercise), ("second",secondExercise), ("third",thirdExercise)]

{-|
  Execution:
  *Lab2> printQuicksortStronger

  Result:
  ["first", "third", "second"]

  So, the result shows that the first predicate is the strongest one, followed by the third predicate.
  The weakest one is the second predicate.
-}


{---------- Assignment 4 ----------}

-- Permutation 1h
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys = (sort xs) == (sort ys)

doPermutation :: [a] -> [a]
doPermutation [] = []
doPermutation xs
  | (length xs == 1) = xs
  | otherwise = head(tail (permutations xs))

doTestPermutation = testR 1 100 doPermutation isPermutation

{-|
  Execution:
  *Lab2> doTestPermutation

  Result:
  "100 tests passed"

  The result of the execution shows each individual test it passed and at the end it states how many tests were actually passed. 
  Every execution of the doTestPermutation method, it passes all the tests, so it can be concluded that the isPermutation method is working correclty.
-}


{---------- Assignment 5 ----------}

isDerangement :: (Eq a, Ord a) => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && forall (zip xs ys) (\(x, y) -> x /= y)

{-|
  Execution:
  *Lab2> isDerangement [0,1,2,3] [3,2,1,0]

  Result:
  True

  The results show that the second list is a derangement of the first list.
-}

deran :: (Eq a, Ord a) => [a] -> [[a]]
deran xs = filter (isDerangement xs) (permutations xs)

{-|
  Execution:
  *Lab2> deran [0,1,2,3]

  Result:
  [[3,2,1,0],[2,3,1,0],[1,2,3,0],[3,0,1,2],[1,3,0,2],[1,0,3,2],[3,2,0,1],[2,3,0,1],[2,0,3,1]]

  The deran method returns all the permutations of the provided list which are also a derangement of it.
-}

deranI :: [Integer] -> [[Integer]]
deranI = deran  -- Type the function because otherwise QuickCheck does not know what to do.
testDeran = quickCheckResult (\xs -> length xs <= 5 --> (foldr (&&) True $ map (isDerangement xs) (deranI xs)))

{-|
  Execution:
  testDeran

  Result:
  +++ OK, passed 100 tests.
  Success {numTests = 100, numDiscarded = 0, labels = fromList [([],100)], classes = fromList [], tables = fromList [], output = "+++ OK, passed 100 tests.\n"}

  The execution always passes the tests of quickCheck, therefore the deran function is valid.
-}


{---------- Assignment 6 ----------}

-- Use the ASCII codes to compute the +13 character.
rot13 :: String -> String
rot13 s = map f s
    where f c | ord c > 128             = c
              | isLetter c && isUpper c = chr (((ord c - 65 + 13) `mod` 26) + 65)
              | isLetter c              = chr (((ord c - 97 + 13) `mod` 26) + 97)
              | otherwise               = c

{-|
  Execution:
  *Lab2> rot13 "this is a test 123"

  Result:
  "guvf vf n grfg 123"

  The result of the execution is a string with the +13th character for each letter in the input string.
-}

testRot13 :: String -> Bool
testRot13 s = rot13 (rot13 s) == s

autoTestRot13 :: IO Result
autoTestRot13 = quickCheckResult testRot13

-- Result: The test succeeds
{-|
  Execution:
  *Lab2> autoTestRot13

  Result:
  +++ OK, passed 100 tests.
  Success {numTests = 100, numDiscarded = 0, labels = fromList [([],100)], classes = fromList [], tables = fromList [], output = "+++ OK, passed 100 tests.\n"}

  The execution tests if twice the rot13 of the sample string is equal to the original string.
  It will always pass the tests, so the rot13 method is valid.
-}


{---------- Assignment 7 ----------}

iban :: String -> Bool
iban = ibanStep4 . ibanStep3 . ibanStep2 . ibanStep1

ibanStep1 :: String -> String
ibanStep1 s = drop 4 s ++ take 4 s  -- Moves the first 4 characters to the end

ibanStep2 :: String -> String -- Replace letters with two digits
ibanStep2 s = concat $ map f s
    where f c | isDigit c = [c]
              | otherwise = show (ord c - 55)

ibanStep3 :: String -> Integer -- Interprets the string as an int
ibanStep3 = read

ibanStep4 :: Integer -> Bool
ibanStep4 x = x `mod` 97 == 1 -- Check whether the modulo of the int equals 1

{-|
  Execution:
  *Lab2> iban "AT483200000012345864"

  Result:
  True

  The execution will return a boolean based on whether the iban is a valid one or not.
  In the above case it is a valid iban, so it returns true.
-}

ibanTestSet, ibanWrongTestSet :: [String]
ibanTestSet = ["AT483200000012345864", "NL79INGB0001611514", "BH02CITI00001077181611", "BG18RZBB91550123456789", "HR1723600001101234565", "DE91100000000123456789", "GB82WEST12345698765432", "GL8964710123456789", "LU120010001234567891", "ES7921000813610123456789"]
ibanWrongTestSet = ["AT403200000012345862", "NL79INGB0001611515", "BH02CITI00001075181611", "BG18RZBB91550123456788", "HR1723600001101233565", "DE91100000000123256789", "GB82WEST12345698764432", "GL8964710123456889", "LU120010001224567891", "ES7921000813611123456789"]
testIban, testWrongIban :: Bool
testIban = foldr (&&) True (map iban ibanTestSet) -- Test iban for all elements in the test set
testWrongIban = foldr (&&) True (map (not . iban) ibanWrongTestSet)

{-|
  Execution:
  *Lab2> testIban
  *Lab2> testWrongIban

  Result:
  True
  True

  The execution will return a boolean based on whether all tests are correct to the validness of the iban.
  The ibans are pre-generated in two list, one with valid ibans and one with invalid ones.
-}