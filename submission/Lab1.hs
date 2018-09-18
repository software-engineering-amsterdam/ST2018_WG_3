
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

testForm4WithImplication = quickCheckResult $ (\xs -> length xs <= 20 --> form4 xs == form4' xs)

{-- The property is hard to test because subsequences gets very slow with lists longer than 20 elements.
    To get around I made a custom QuickCheck generator to just produce short lists. Because I'm not sure
    how this should be done, I made a generator that only produces lists of a fixed size of 20 elements.
    Then I was told we could also use implications so I also made a version with that. --}

{-- The test will only succeed if all the components are right (or wrong in just the right way, which has
    only a very small chance of happening). The mathematical facts need to be right, the subsequences
    function needs to be correct, but the test itself should also not contain any errors. --}


-- Assignment 3
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

-- -- Assignment 4 -- --

reversePrimes :: [Integer]
reversePrimes = filter (prime . reversal) xs
  where xs = takeWhile (\x -> x <= 10000) primes

{-- You may test this by checking that every element in reversePrimes is a prime and its reverse is also
    a prime. The problem with this method is that this does not check for exhaustiveness. --}

-- Exercise 5
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

-- Exercise 6
calculateConjeture :: Int -> Bool
calculateConjeture n = prime ((product(take n primes)) + 1)

counterExamples :: Int -> [Integer]
counterExamples n
  | calculateConjeture n = counterExamples (n+1)
  | otherwise = take n primes

doCounterExamples :: Integer
doCounterExamples = last (counterExamples 2)

-- Exercise 7
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

-- Exercise 8
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
honest = filter (\x -> accuses x (head guilty)) boys
