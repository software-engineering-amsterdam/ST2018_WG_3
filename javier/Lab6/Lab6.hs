------- Javier Bermejo Razquin -------
module Lab6

where

import Lecture6
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck

-------- Exercise 1 - 2h ----------
{--
b = base
e = exponent
m = mod
--}
{--exM' b e m =
  if e == 1 then ((1*b) `mod` m)
    else ((exM' b (e-1) m)*b) `mod` m--}

exM' :: Integer -> Integer -> Integer -> Integer
exM' b e m =
  doExM' b (reverse(toBin'(e))) m 1

--https://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell

toBin' 0 = [0]
toBin' n = reverse (helper' n)

helper' 0 = []
helper' n | n `mod` 2 == 1 = 1 : helper' (n `div` 2)
          | n `mod` 2 == 0 = 0 : helper' (n `div` 2)

doExM' :: Integer -> [Integer] -> Integer -> Integer -> Integer
doExM' b [] m r = r
doExM' b e m r =
  if (head e) == 1 then doExM' ((b*b) `mod` m) (tail e) m ((r*b) `mod` m)
    else doExM' ((b*b) `mod` m) (tail e) m r

---------- Exercise 2 - 2:30h ---------

generateRandomExample1 :: IO(Integer)
generateRandomExample1 =
  do return (exM' 20000 43000000 301)

generateRandomExample2 :: IO(Integer)
generateRandomExample2 =
  do return (expM 20000 43000000 301)

-- We do :set +s before doing these 2 functions
-- generateRandomExample1: (0.00 secs, 105,384 bytes)
-- generateRandomExample2: (9.48 secs, 215,501,616 bytes)


---------- Exercise 3 - 30 min------------
composites' :: [Integer]
composites' = filter (\x -> not (prime x)) [2..]


---------- Exercise 4 - 35 mins ------------
doTestsFermat1 :: IO Integer
doTestsFermat1 = testsFermat1 composites

testsFermat1 :: [Integer] -> IO Integer
testsFermat1 xs = do
  b <- primeTestsF 1 (head xs)
  if (not b) then testsFermat1 (tail xs)
    else return (head xs)

doTestsFermat2 :: IO Integer
doTestsFermat2 = testsFermat2 composites

testsFermat2 :: [Integer] -> IO Integer
testsFermat2 xs = do
  b <- primeTestsF 2 (head xs)
  if (not b) then testsFermat2 (tail xs)
    else return (head xs)

doTestsFermat3 :: IO Integer
doTestsFermat3 = testsFermat3 composites

testsFermat3 :: [Integer] -> IO Integer
testsFermat3 xs = do
  b <- primeTestsF 3 (head xs)
  if (not b) then testsFermat3 (tail xs)
    else return (head xs)

doTestsFermatN :: Int -> IO Integer
doTestsFermatN n = testsFermatN n composites

testsFermatN :: Int -> [Integer] -> IO Integer
testsFermatN n xs = do
  b <- primeTestsF n (head xs)
  if (not b) then testsFermatN n (tail xs)
    else return (head xs)

--- If K is big, is going to take more time to compute, but also it will give
--- better results. If the k is big, the function will be less fooled
--- Nonetheless, at one point there is always a composite number that the
--- function thinks that is a prime


---------- Exercise 5 - 20 mins ------------
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

doTestsFermatCarmichaelN :: Int -> IO Integer
doTestsFermatCarmichaelN n = testsFermatN n carmichael

--- The first number fooled is normally 294409
--- If you put a big k (+10), sometimes that number goes up (56052361)


----------- Exercise 6 - 30 mins --------------
doTestsMR1 :: IO Integer
doTestsMR1 = testsMR1 carmichael

testsMR1 :: [Integer] -> IO Integer
testsMR1 xs = do
  b <- primeMR 1 (head xs)
  if (not b) then testsMR1 (tail xs)
    else return (head xs)

doTestsMR2 :: IO Integer
doTestsMR2 = testsMR2 carmichael

testsMR2 :: [Integer] -> IO Integer
testsMR2 xs = do
  b <- primeMR 2 (head xs)
  if (not b) then testsMR2 (tail xs)
    else return (head xs)

doTestsMR3 :: IO Integer
doTestsMR3 = testsMR3 carmichael

testsMR3 :: [Integer] -> IO Integer
testsMR3 xs = do
  b <- primeMR 3 (head xs)
  if (not b) then testsMR3 (tail xs)
    else return (head xs)

doTestsMRN :: Int -> IO Integer
doTestsMRN n = testsMRN n carmichael

testsMRN :: Int -> [Integer] -> IO Integer
testsMRN n xs = do
  b <- primeMR n (head xs)
  if (not b) then testsMRN n (tail xs)
    else return (head xs)


--- This method works way better than previous ionodes
--- doTestsMR1 : 228842209
--- doTestsMR2 : 527519713969
--- doTestsMR3 : 105950928237841
--- doTestsMRN 4 : 37041433670517160249
--- doTestsMRN 5 : Toke too much timeItT


------------- Exercise 6 (again) 35 mins -----------
doDiscoverMarsenne :: Integer -> IO [Integer]
doDiscoverMarsenne n = discoverMarsenne n primes []

discoverMarsenne :: Integer -> [Integer] -> [Integer] -> IO [Integer]
discoverMarsenne n xs ys= do
  b <- primeMR 2 ((2^(head xs))-1)
  if (n>0) then
    if b then discoverMarsenne (n-1) (tail xs) (ys++[head xs])
      else discoverMarsenne n (tail xs) ys
  else
    return ys

--- It finds marsenne numbers
--- I put in primeMR 2, because 1 is not good enough and 3 is too solveShowNs
--- I was able to find up to 21 numbers, taking a total of 206.93 seconds to
--- compute
--- The numbers were : [2,3,5,7,13,17,19,31,61,89,107,127,521,607,1279,2203,
---                     2281,3217,4253,4423,9689]
