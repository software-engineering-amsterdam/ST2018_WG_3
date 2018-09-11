
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

-- Exercise 4 (1:30H)
primeReversal :: Integer -> [Integer]
primeReversal n = [x | x <- [1..n], let b = reversal x, prime x && prime b]

notPrime :: Integer -> Bool
notPrime x = not (prime x)

--primeReversalTest (x:xs) =
--if notPrime x
--dont know how to do it

-- Exercise 5 (40 Mins)
sum101Primes :: [Integer] -> Integer
sum101Primes xs
  | prime (sum(take 101 xs)) = sum(take 101 xs)
  | otherwise = sum101Primes (drop 1 xs)

doSum101Primes :: Integer
doSum101Primes = sum101Primes primes

-- We are not going to test this function, as we use library Functions
-- (that we asume that work), and functions that have been already tested
-- (like for example, prime or primes). We are taking a list of primes, and
-- summing the 101 first results. If this doesn't give a prime number, then
-- we remove the first in the list, and we sum the next 101 numbers. We repeat
-- this till we find the prime number

-- Exercise 6 (30 Mins)
calculateConjeture :: Int -> Bool
calculateConjeture n = prime ((product(take n primes)) + 1)

counterExamples :: Int -> [Integer]
counterExamples n
  | calculateConjeture n = counterExamples (n+1)
  | otherwise = take n primes

doCounterExamples :: [Integer]
doCounterExamples = counterExamples 2

-- Exercise 7 (2h without testing)
intToDigs :: Int->[Int]
intToDigs 0 = []
intToDigs x = intToDigs (x `div` 10) ++ [x `mod` 10]

--Taken from here https://stackoverflow.com/questions/19867491/double-every-other-element-of-list-from-right-in-haskell
--I had the same idea, only i didn't know the syntax
doublePairs :: [Int] -> [Int]
doublePairs xs = fst (foldr (\x (numbers,bool) ->
                              ((if bool then 2 * x else x) : numbers, not bool))
                              ([], False) xs)

sumBigNumbers :: [Int] -> [Int]
sumBigNumbers xs = [sum (intToDigs x) | x <- xs]

sumNumbers :: [Int] -> Int
sumNumbers xs = sum xs

luhn :: Int -> Bool
luhn x = sumNumbers(sumBigNumbers(doublePairs (intToDigs x))) `mod` 10 == 0

checkVisa :: Int -> Bool
checkVisa x = x `div` 1000000000000000 == 4

isVisa :: Int -> Bool
isVisa x = (checkVisa x) && (luhn x)

checkMaster :: Int -> Bool
checkMaster x = (x `div` 100000000000000) `elem` [51..55]

isMaster :: Int -> Bool
isMaster x = (checkMaster x) && (luhn x)

checkAmericanExpress :: Int -> Bool
checkAmericanExpress x = (x `div` 100000000000000) `elem` [34,37]

isAmericanExpress :: Int -> Bool
isAmericanExpress x = (checkAmericanExpress x) && (luhn x)

-- We only have to test luhn implementation, as the other functions are
-- basic lisp implementations
-- To test the luhn implementation, we are going to generate luhn numbers and
-- see if it works

--luhnGenerator :: Int
--luhnGenerator = [100000000000000..999999999999999]


-- Exercise 8 (2h)
-- We define xor for the accuses function
xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

-- We define who accuses everyone, using the last function created
accuses :: Boy -> Boy -> Bool
accuses Matthew x = not (x == Matthew) && not (x == Carl)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = (accuses Matthew x) `xor` (accuses Peter x)
accuses Carl x = not (accuses Arnold x)

-- We want to know the list of which person is accused by
accusers :: Boy -> [Boy]
accusers boy = filter (\x -> accuses x boy) boys

-- We want to know who is guilty.
-- We know that only 3 people are telling the truth, so if we verify that
-- We will know who was guilty
guilty :: [Boy]
guilty = filter (\x -> length (accusers x) == 3) boys

--We want to know who said the truth
-- We know that 2 people are saying lies, and they dont accuse Jack
-- We know that 3 people said the truth and accused Jack
honest :: [Boy]
honest = filter (\x -> accuses x Jack) boys
