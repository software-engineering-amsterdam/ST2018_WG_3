
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


--First exercise ~2 hours

workshop1 :: Int -> Int
workshop1  n = sum $ take ( n + 1 ) $ map ( \x -> x * x ) [0..]
 

workshop1' :: Int -> Int
workshop1' x = ( x* ( x+1 ) * ( 2*x+1 ) ) `div` 6

testing x = x >= 0 --> workshop1 x == workshop1' x


workshop2 :: Int -> Int
workshop2 n = sum $ take ( n + 1 ) $ map ( \x -> x ^ 3 ) [0..]

workshop2' :: Int -> Int
workshop2' x = ( ( x * ( x + 1 ) ) `div` 2 ) ^ 2

testing2 x = x >= 0 --> workshop2 x == workshop2' x
 

--Second exercise ~ 1 hour 

workshop3 :: [Int] -> Int
workshop3 n = length ( subsequences n )

workshop3' :: [Int] -> Int
workshop3' m = 2 ^ length ( m )

testing3 x = (( length x ) < 10) --> workshop3 x == workshop3' x













