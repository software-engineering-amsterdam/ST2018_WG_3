
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


--------------------LECTURE  MATERIAL ------------
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

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

------------------ ACTUAL EXERCISES -----------------

-- Exercise test probs 4:35
-- Precondition
-- The property isTrue that holds of any.
isTrue :: a -> Bool
isTrue _ = True

-- Postcondition
-- 1/4 of all numbers have to be in their corresponding quartiles
prob :: (Ord a, Fractional a) => [a] -> [Int] -> [Int]
prob [] [a,b,c,d] = [a,b,c,d]
prob (x:xs) [a,b,c,d]
  | x < 0.25 = prob xs [a+1,b,c,d]
  | x < 0.5  = prob xs [a,b+1,c,d]
  | x < 0.75 = prob xs [a,b,c+1,d]
  | x < 1    = prob xs [a,b,c,d+1]
  | otherwise = [a,b,c,d]

probToBool :: (Ord a, Fractional a) => [a] -> a -> Bool
probToBool [] _ = True
probToBool xs total
  | (((head xs) / total) >= 0.20) && (((head xs) / total) <= 0.30) = probToBool (tail xs) total
  | otherwise = False

{--testProb :: (Ord a, Fractional a) => Int -> Int -> ([a] -> [Int] -> [Int]) -> ([a] -> a -> Bool) -> Int -> IO()
testProb k n f r numbers = if k == n then print (show n ++ " Tests Passed")
                    else do
                        xs <- probs numbers
                        if r (f xs [0,0,0,0]) numbers then
                            do testProb (k+1) n f r numbers
                        else error ("failed on " ++ show xs)



doTestProb = testProb 0 100 prob probToBool 10000--}





-- Recognizing triangles 1h spent
-- This function puts the list in descending order
orderSides :: Integer -> Integer -> Integer -> [Integer]
orderSides a b c = reverse (sort [a,b,c])

--This function checks the type of the triangle
checkType :: [Integer] -> Shape
checkType [a,b,c]
  | a > (b+c) = NoTriangle
  | a^2 == (b^2 + c^2) = Rectangular
  | a == b && a == c && b == c = Equilateral
  | a == b || a == c || b == c = Isosceles
  | otherwise = Other

--This function checks the type of a triangle after putting the list in a descending order
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = checkType (orderSides a b c)

{-- To test this program we have to see if the order of the numbers is important
 and checking that triangles that we already know are detected correctly --}

-- This function checks that all combinations of numbers give the same triangle
allSameTriangles :: Integer -> Integer -> Integer -> Bool
allSameTriangles a b c = (triangle a b c) == (triangle b c a) && (triangle a b c) == (triangle c a b)

-- This function tests several different examples
testTriangles :: Bool
testTriangles
  | not (allSameTriangles 3 4 5 --> triangle 3 4 5 == Rectangular) = False
  | not (allSameTriangles 3 3 3 --> triangle 3 3 3 == Equilateral) = False
  | not (allSameTriangles 3 3 4 --> triangle 3 3 4 == Isosceles) = False
  | not (allSameTriangles 56 1 1 --> triangle 56 1 1 == NoTriangle) = False
  | otherwise = True

{-- The result from the tests is that the function works--}



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



-- Derangements 30 mins
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = False
isDerangement xs ys
  | (length xs) /= (length ys) = False
  | (head xs) /= (head ys) = isDerangement (tail xs) (tail ys)
  | otherwise = True

deran :: Eq a => [a] -> [[a]]
deran xs = [ ys | ys <- (permutations xs), isDerangement xs ys]

-- ROT13 encoding 1h
--We create a list for selecting next character
lowerCaseCustom, upperCaseCustom :: Char -> [Char]
lowerCaseCustom x = [x..'z'] ++ ['a'..'z']
upperCaseCustom x = [x..'Z'] ++ ['A'..'Z']

--We get the next N character
getNextNChar :: Char -> Int -> Char
getNextNChar x n
  | x == ' ' = ' '
  | isUpper x = head (drop n (upperCaseCustom x))
  | otherwise = head (drop n (lowerCaseCustom x))

-- We create the string
rot13 :: String -> String
rot13 xs = [ getNextNChar x 13 | x <- xs]

-- To test that it works, we use two times rot13 in order to have the original
-- string. The result it's true, it works
propRot13 :: String -> Bool
propRot13 xs = xs == rot13(rot13 xs)

-- IBAN validation 2h
-- Only working for UK
checkLength :: [a] -> Bool
checkLength xs = (length xs) == 22

move4Digits :: [Char] -> [Char]
move4Digits xs = drop 4 xs ++ take 4 xs

letterToNumber = zip ['A'..'Z'] [10..35]
replaceLetterByNumber :: Char -> [(Char, Int)] -> Int
replaceLetterByNumber x xs = if (x == fst (head xs)) then snd (head xs)
                                else replaceLetterByNumber x (tail xs)

replaceStringByList :: [Char] -> [Int]
replaceStringByList xs = [replaceLetterByNumber x letterToNumber | x <- xs, isUpper x]

replaceListByNumber :: [Int] -> Int -> Int
replaceListByNumber [] n = n
replaceListByNumber (x:xs) n = x + (replaceListByNumber xs n)

calculateMod :: Int -> Bool
calculateMod n = (n `mod` 97) == 1

iban :: String -> Bool
iban xs = if checkLength xs
            then calculateMod(replaceListByNumber(replaceStringByList (move4Digits xs)) 0)
            else False
