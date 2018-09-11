
module Lab1 where
import Data.List
import Test.QuickCheck
import System.Random

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


------ Actual exercises ------

-- -- Assignment 1 -- --


quickCheckEqualF f f' = quickCheckResult (\x -> f x == f' x)

-- Prove exercise 2 with QuickTest

form2 :: Positive Int -> Int
form2 (Positive n) = sum $ take (n+1) $ map (\ x -> x * x) [0..]

form2' :: Positive Int -> Int
form2' (Positive n) = (n * (n + 1) * (2 * n + 1)) `div` 6

testForm2 = quickCheckResult (\x -> form2 x == form2' x)

-- Prove exercise 3 with QuickTest

form3 :: Positive Int -> Int
form3 (Positive n) = sum $ take (n+1) $ map (\ x -> x ^ 3) [0..]

form3' :: Positive Int -> Int
form3' (Positive n) = ((n * (n + 1)) `div` 2) ^ 2

testForm3 = quickCheckEqualF form3 form3'


-- -- Assignment 2 -- --

form4 :: [Int] -> Int
form4 xs = length $ subsequences xs

form4' :: [Int] -> Int
form4' xs = 2 ^ length xs

smallLists :: Gen [Int]
smallLists = replicateM 20 (choose (0, 100))

testForm4WithMonad = quickCheckResult $ forAll smallLists (\xs -> form4 xs == form4' xs)

testForm4WithImplication = quickCheckResult $ (\xs -> length xs <= 20 --> form4 xs == form4' xs)

{-- The property is hard to test because subsequences gets very slow with lists longer than 20 elements.
    To get around I made a custom QuickCheck generator to just produce short lists. Because I'm not sure
    how this should be done, I made a generator that only produces lists of a fixed size of 20 elements.
    Then I was told we could also use implications so I also made a version with that. --}

{-- The test will only succeed if all the components are right (or wrong in just the right way, which has
    only a very small chance of happening). The mathematical facts need to be right, the subsequences
    function needs to be correct, but the test itself should also not contain any errors. --}


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

testPermsProp = quickCheckResult test where
     test :: [Int] -> Bool
     test xs = let n = length xs
               in n <= 10 --> factorial n == length (perms xs)

{-- The property is as hard to test as the last one, because the computation is again very expensive.
    Therefore I have limited the list length to a maximum of 10. --}
