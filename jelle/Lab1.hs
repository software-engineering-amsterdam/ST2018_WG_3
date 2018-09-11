
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

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- -- Exercise 1 (45 min) -- --

-- Redo workshop exercise 2
workshop2 :: Int -> Int
workshop2 n = sum [x^2 | x <- [1..n]]

workshop2' :: Int -> Int
workshop2' n = (n * (n + 1) * (2 * n + 1)) `div` 6

test2 = (\x -> x > 0 --> workshop2 x == workshop2' x)

-- Redo workshop exercise 3
workshop3 :: Int -> Int
workshop3 n = sum [x^3 | x <- [1..n]]

workshop3' :: Int -> Int
workshop3' n = ((n * (n + 1)) `div` 2)^2

test3 = (\x -> x > 0 --> workshop3 x == workshop3' x)

-- Exercise 2 (1h)
{-|
	It is quite hard to test this property, since the numbers get really large and therefore it provides a stack overflow.
	Testing this property is about testing the mathematical fact that a powerset grows exponentionally.
-}
workshop4 :: Int -> Int
workshop4 n = length (subsequences [1..n])

workshop4' :: Int -> Int
workshop4' n = 2^n

test4 = (\x -> x > 0 && x < 20 --> workshop4 x == workshop4' x)

-- Exercise 3 (25 min...)
workshop5 :: Int -> Int
workshop5 n = length $ perms [1..n]

factorial :: Int -> Int
factorial n = product [1..n]

test5 = (\x -> x > 0 && x < 10 --> workshop5 x == factorial x)

-- Exercise 4 (1h)
{-|
	
-}
reversePrimes = [x | x <- xs, prime (reversal x)]
    where xs = takeWhile (\x -> x <= 10000) primes

reversePrimes' = filter (\x -> prime (reversal x)) xs
    where xs = takeWhile (\x -> x <= 10000) primes

-- Exercise 5 (2h)

primes' :: Integer -> [Integer]
primes' n = filter prime [n..]

consecutive101 n = sum $ take 101 $ primes' n
consecutive' = take 1 [consecutive101 x | x <- primes, prime $ consecutive101 x]

-- Exercise 6
conjecture n = prime $ (product $ take n primes) + 1
counter = takeWhile (\x -> conjecture x) [2..]

-- Exercise 7 (1h)
digits :: Integer -> [Int]
digits = map (read . return) . show

luhn :: Integer -> [Int]
luhn n = [(snd x) * 2 | x <- xs, fst x `mod` 2 == 0]
    ++[snd x | x <- xs, fst x `mod` 2 /= 0] 
    where xs = zip [1..] $ digits n