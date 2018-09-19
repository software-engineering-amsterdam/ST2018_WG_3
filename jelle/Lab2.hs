
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

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



-- -- Question 1 -- -- (1h)
testProbs = do result <- probs 10000
               q1 <- return $ filter (\x -> x > 0 && x <= 0.25) result
               q2 <- return $ filter (\x -> x > 0.25 && x <= 0.5) result
               q3 <- return $ filter (\x -> x > 0.5 && x <= 0.75) result
               q4 <- return $ filter (\x -> x > 0.75 && x <= 1) result
               return $ map length [q1,q2,q3,q4]
{-|
    *Lab2> testProbs
    [2561,2447,2539,2453]
-}

-- -- Question 2 -- -- (5h)
isTriangle a b c = (a + b) >= c && (a + c) >= b && (b + c) > a
isEquilateral a b c = isTriangle a b c && a == b && a == c && b == c
isIsosceles a b c = isTriangle a b c && (a == b || a == c || b == c) && not (isEquilateral a b c)
isRectangular a b c = isTriangle a b c && 
    (a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2)

getRandomInt n = getStdRandom (randomR (1,n))

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | not $ isTriangle a b c = NoTriangle
    | isEquilateral a b c = Equilateral
    | isIsosceles a b c = Isosceles
    | isRectangular a b c = Rectangular
    | otherwise = Other

preNoTriangle a b c = a > (b + c) || b > (a + c) || c > (a + b)
preEquilateral a = isTriangle a a a
preIsosceles a b = isTriangle a a b && a /= b
preRectangular a b c = isTriangle a b c && (a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2)
preOther a b c = isTriangle a b c && a /= b && b /= c && a /= c && not (preRectangular a b c)

{-|
    k = start point of tests - usually 1
    n = number of total tests
    m = maximum number of randomInt for point of triangle
-}
-- Some of the samples don't statisfy the precondition, which limits the number of "real" tests
testNoTriangle k n m = if k == n then print (show n ++ " tests passed")
                       else do a <- getRandomInt m
                               b <- getRandomInt m
                               c <- getRandomInt m
                               if preNoTriangle a b c --> (triangle a b c == NoTriangle) then
                                testNoTriangle (k+1) n m
                               else error ("failed test on: " ++ show a ++ ", " ++ show b ++ ", " ++ show c)

testEquilateral k n m = if k == n then print (show n ++ " tests passed")
                        else do a <- getRandomInt m
                                if preEquilateral a --> (triangle a a a == Equilateral) then
                                    testEquilateral (k+1) n m
                                else error ("failed test on: " ++ show a ++ ", " ++ show a ++ ", " ++ show a)

testIsosceles k n m = if k == n then print (show n ++ " tests passed")
                      else do a <- getRandomInt m
                              b <- getRandomInt m
                              if preIsosceles a b --> (triangle a a b == Isosceles) then
                                testIsosceles (k+1) n m
                              else error ("failed test on: " ++ show a ++ ", " ++ show a ++ ", " ++ show b)

testRectangular k n m = if k == n then print (show n ++ " tests passed")
                        else do a <- getRandomInt m
                                b <- getRandomInt m
                                c <- getRandomInt m
                                if preRectangular a b c --> (triangle a b c == Rectangular) then
                                    testRectangular (k+1) n m
                                else error ("failed test on: " ++ show a ++ ", " ++ show b ++ ", " ++ show c)

testOther k n m = if k == n then print (show n ++ " tests passed")
                        else do a <- getRandomInt m
                                b <- getRandomInt m
                                c <- getRandomInt m
                                if preOther a b c --> (triangle a b c == Other) then
                                    testOther (k+1) n m
                                else error ("failed test on: " ++ show a ++ ", " ++ show b ++ ", " ++ show c)


-- -- Question 3 -- -- (2h)
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 


-- (a) --
first, second, third :: Int -> Bool
first x = even x && x > 3
second x = even x || x > 3
third x = (even x && x > 3) || even x

-- (b) --
quicksortProperties [] = []
quicksortProperties (x:xs) =
    quicksortProperties [ a | a <- xs, stronger [-10..10] (fst a) (fst x)]
    ++ [x]
    ++ quicksortProperties [ a | a <- xs, weaker [-10..10] (fst a) (fst x)]

orderProperties p = map snd $ quicksortProperties p
exampleOrderProperties = orderProperties [(first, "first"), (second, "second"), (third, "third")]

{-|
    Execution of the method:
    *Lab2> orderProperties [(first, "first"), (second, "second"), (third, "third")]
    *Lab2> exampleOrderProperties

    Result:
    ["first","third","second"]
-}

-- Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = a `elem` permutations b

isPermutation' :: Ord a => [a] -> [a] -> Bool
isPermutation' [] [] = True
isPermutation' a b = (sort a) == (sort b)

samplePermutation :: [a] -> IO [a]
samplePermutation x = do xs <- return $ permutations x
                         n <- getRandomInt $ (length xs)-1
                         p <- return $ xs !! n
                         return p

-- TODO: make a test for the permutations                        

-- -- Question 4 -- --
{-|
    Conditions:
        - length of lists are equal
        - Lists contains the same items
        --> To work with duplicates it works to sort both lists
-}
isDerangement x y = isPermutation x y && forall (zip x y) (\(x, y) -> x /= y)

deran x = filter (isDerangement x) y 
    where y = permutations x