
module Lab1 where
import Data.List
import Control.Monad
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



-- -- Assignment 1 (30m) -- --

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
    

-- -- Assignment 3 -- --

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
reversePrimes = filter (prime . reversal) primes

{-- You may test this by checking that every element in reversePrimes is a prime and its reverse is also
    a prime. The problem with this method is that this does not check for exhaustiveness. --}


-- -- Assignment 5 -- --

fivePrimes :: [Integer]
fivePrimes = let combinations = map (\x -> [primes !! x, primes !! (x+1), primes !! (x+2), primes !! (x+3), primes !! (x+4)]) [0..] in
    filter prime $ map sum combinations
    
lotsOfPrimes :: Int -> [Integer]
lotsOfPrimes n = let combinations = map (\i -> take n $ drop i primes) [0..] in
    filter prime $ map sum combinations
    
{-- I don't know how to test this --}


-- -- Assignment 6 -- --

conjecture :: Int -> Bool
conjecture n = prime $ product consecutivePrimes + 1
    where consecutivePrimes = take n primes

cCounterexample :: Int
cCounterexample = maybe (-1) ((+)1) $ findIndex (not . conjecture) [1..]


-- -- Assignment 7 -- --

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f f' (x:xs) = f x : altMap' f f' xs
    where altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
          altMap' _ _ [] = []
          altMap' f f' (x:xs) = f' x : altMap f f' xs

digits :: Integer -> [Integer]
digits x = map (\d -> read [d]) (show x)

luhn :: Integer -> Bool
luhn x = sum opdDigits `mod` 10 == 0
    where opdDigits :: [Integer]
          opdDigits = reverse $ altMap id doubleOp $ reverse $ digits x
          doubleOp :: Integer -> Integer
          doubleOp x = if x<5 then x*2 else sum (digits (x*2))

hasIIN :: [Integer] -> Integer -> Bool
hasIIN iins x = foldr (\iin r -> r || take (length $ iin) (digits x) == iin) False (map digits iins)

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress x = luhn x && hasIIN [34, 37] x
isVisa x = luhn x && hasIIN [4] x
isMaster x = luhn x && hasIIN ([51..55] ++ [2221..2720]) x


-- -- Assignment 8 -- --

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]