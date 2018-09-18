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


{-- Assignment 1 --}

testProbs :: Int -> IO [Int]
testProbs n = do
    results <- probs n
    fstQ <- return $ filter (\x -> x >= 0 && x < 0.25) results
    sndQ <- return $ filter (\x -> x >= 0.25 && x < 0.5) results
    thdQ <- return $ filter (\x -> x >= 0.5 && x < 0.75) results
    fthQ <- return $ filter (\x -> x >= 0.75 && x < 1) results
    return $ map length [fstQ, sndQ, thdQ, fthQ]
    
-- Result: a list of four numbers that are all around 1/4 of n. The higher n, the closer
-- they are (percentually) to the 1/4 point.


{-- Assignment 2 --}

isTriangle, isEquilateral, isRectangular, isIsosceles :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a+b>=c && a+c>=b && b+c>=a     -- 
isEquilateral a b c = a == b && b == c         -- The triangle is equilateral if all sides are the same length.
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
    
-- Result: All the tests return true


{-- Assignment 3 --}

-- Testing properties strength 1h30
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

firstExercise :: Int -> Bool
firstExercise x = (even x && x > 3)

secondExercise :: Int -> Bool
secondExercise x = (even x || x > 3)

thirdExercise :: Int -> Bool
thirdExercise x = (firstExercise x || even x)

quicksortStronger :: (Num a1, Enum a1) => [(a2, a1 -> Bool)] -> [(a2, a1 -> Bool)]
quicksortStronger [] = []
quicksortStronger (x:xs) =
   quicksortStronger [ p | p <- xs, stronger [-10..10] (snd p) (snd x)]
   ++ [x]
   ++ quicksortStronger [ q | q <- xs, weaker [-10..10] (snd q) (snd x)]

printQuicksortStronger :: [String]
printQuicksortStronger =
    map fst $ quicksortStronger [("first",firstExercise), ("second",secondExercise), ("third",thirdExercise)]

-- The result is: ["first","third","second"].


{-- Assignment 4 --}

-- Permutation 1h
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys = (sort xs) == (sort ys)

doPermutation :: [a] -> [a]
doPermutation [] = []
doPermutation xs
  | (length xs == 1) = xs
  | otherwise = head(tail (permutations xs))

doTestPermutation = testR 1 100 doPermutation isPermutation -- Everything is correct; result is True


{-- Assignment 5 --}

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && forall (zip xs ys) (\(x, y) -> x /= y)

deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) (permutations xs)
deranI :: [Integer] -> [[Integer]]
deranI = deran  -- Type the function because otherwise QuickCheck does not know what to do.
testDeran = quickCheckResult (\xs -> length xs <= 5 --> (foldr (&&) True $ map (isDerangement xs) (deranI xs)))

-- Result: The test succeeds


{-- Assignment 6 --}

-- Use the ASCII codes to compute the +13 character.
rot13 :: String -> String
rot13 s = map f s
    where f c | ord c > 128             = c
              | isLetter c && isUpper c = chr (((ord c - 65 + 13) `mod` 26) + 65)
              | isLetter c              = chr (((ord c - 97 + 13) `mod` 26) + 97)
              | otherwise               = c

testRot13 :: String -> Bool
testRot13 s = rot13 (rot13 s) == s

autoTestRot13 :: IO Result
autoTestRot13 = quickCheckResult testRot13

-- Result: The test succeeds


{-- Assignment 7 --}

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

ibanStep3 :: String -> Integer -- Interprets the string as an int
ibanStep3 = read

ibanStep4 :: Integer -> Bool
ibanStep4 x = x `mod` 97 == 1 -- Check whether the modulo of the int equals 1


ibanTestSet, ibanWrongTestSet :: [String]
ibanTestSet = ["AT483200000012345864", "NL79INGB0001611514", "BH02CITI00001077181611", "BG18RZBB91550123456789", "HR1723600001101234565", "DE91100000000123456789", "GB82WEST12345698765432", "GL8964710123456789", "LU120010001234567891", "ES7921000813610123456789"]
ibanWrongTestSet = ["AT403200000012345862", "NL79INGB0001611515", "BH02CITI00001075181611", "BG18RZBB91550123456788", "HR1723600001101233565", "DE91100000000123256789", "GB82WEST12345698764432", "GL8964710123456889", "LU120010001224567891", "ES7921000813611123456789"]
testIban, testWrongIban :: Bool
testIban = foldr (&&) True (map iban ibanTestSet) -- Test iban for all elements in the test set
testWrongIban = foldr (&&) True (map (not . iban) ibanWrongTestSet)

-- Result: All the (preprogrammed) tests succeed.
