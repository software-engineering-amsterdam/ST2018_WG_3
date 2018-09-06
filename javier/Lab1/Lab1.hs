
module Lab1 where
import Data.List
import Test.QuickCheck

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


--------------- Actual Exercises ---------------
-- Exercise 1 ( 1:15H )
functionA1 :: Positive Int -> Int
functionA1 (Positive 0) = 0
functionA1 (Positive n) = n*n + functionA1 (Positive (n-1))

functionB1 :: Positive Int -> Int
functionB1 (Positive n) = (n * (n + 1) * ((2*n)+1)) `div` 6

my1Test :: Positive Int -> Bool
my1Test (Positive x) =
  functionA1 (Positive x) == functionB1 (Positive x)

testMy1Test = quickCheck my1Test

functionA2 :: Int -> Int
functionA2 0 = 0
functionA2 n = n^3 + functionA2 (n-1)

functionB2 :: Int -> Int
functionB2 n = ((n*(n+1)) `div` 2) ^ 2

my2Test :: Positive Int -> Bool
my2Test (Positive x) = functionA2 x == functionB2 x

testMy2Test = quickCheck my2Test

-- Exercise 2 (1h)
powerSet :: [a] -> Int
powerSet xs = 2^(length xs)

setCounter :: [a] -> Int
setCounter xs = length (subsequences [1..(length xs)])

mySetTest :: [Int] -> Bool
--mySetTest xs = powerSet xs == setCounter xs

-- Yes, it is hard to test because the computational cost is too high
-- The lenght of the list becomes very high, so it costs a lot of time
-- to calculate everything.
-- In order to be able to do it, we have to limit the size of the
-- lists

mySetTest xs = ((length xs) < 20) --> (powerSet xs == setCounter xs)

testMySetTest = quickCheckResult mySetTest

-- We are testing if subsequences works, having mathematical facts as a proof
-- We are not proving mathematical facts, we are using them in order to
-- test our implementations


-- Exercise 3 (30 minutes)
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
 insrt x [] = [[x]]
 insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

lengthPerms :: [a] -> Int
lengthPerms xs = length (perms xs)

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n-1)

factorialLength :: [a] -> Int
factorialLength xs = factorial' (length xs)

myPermTest :: [Int] -> Bool
-- myPermTest xs = lengthPerms xs == factorialLength xs
testMyPermTest = quickCheckResult myPermTest

-- With this exercise we have the same problem than before, the lists are
-- too long, so the computational power needed to calculate it is too big
-- We do not need long examples to know if it works or not, so we will
-- reduce the length of the lists

myPermTest xs = ((length xs) < 10) --> lengthPerms xs == factorialLength xs

-- We are testing if our perms implementation work. We are using mathematical
-- facts in order to know if this implementation is correct

-- Exercise 4 (30 min)
primeReversal :: Integer -> [Integer]
primeReversal n = [x | x <- [1..n], let b = reversal x, prime x && prime b]

notPrime :: Int -> Bool
notPrime = not prime

primeReversalTest (x:xs) =
  if notPrime x

-- Exercise 5
sum101Primes = 
