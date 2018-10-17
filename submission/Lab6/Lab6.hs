module Lab6 where
  
import Data.List
import Data.Tuple
import Control.Monad
import Control.Monad.Extra
import Control.Arrow
import Data.Maybe
import System.Random
import Test.QuickCheck hiding (forAll)
import Lecture6
import System.TimeIt

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q



{-- Assignment 1 --}

{--

See Lecture6.hs.

Implementation is like this:

exM :: Integer -> Integer -> Integer -> Integer
exM x 0 n = rem 1 n
exM x e n | odd e      = rem (x * (exM x (e-1) n)) n
          | otherwise  = rem (exM x (e `div` 2) n ^ 2) n

--}

-- Testing the function
testExM :: Integer -> Integer -> Integer -> Bool
testExM x e n = n > 0 && e >= 0 --> exM x e n == expM x e n


{-- Assignment 2 --}

compareExps :: Integer -> Integer -> Integer -> IO Bool
compareExps x e n = do
  (time1, _) <- timeItT (putStrLn $ show $ exM x e n)
  (time2, _) <- timeItT (putStrLn $ show $ expM x e n)
  return $ time1 < time2
  
comparisons :: IO Bool
comparisons = (liftM and) $ sequence $
                  map (uncurry3 compareExps)
                      [(21234, 72239412, 301),
                       (10333, 51827932, 279),
                       (12513, 21676128, 212),
                       (124951, 52178394, 315)]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- When running comparisons it returns true, because exM is
-- faster than expM in all the test cases.


---------- Exercise 3 - 30 min------------
composites' :: [Integer]
composites' = filter (not.prime) [2..]


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


----------- Exercise 6 (1) - 30 mins --------------
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
  
-- The fools we find here are much higher than the ones in previous
-- exercises. Not uncommonly, they are larger than 10^11.


{-- Assignment 6 (2) --}

largeMersennes :: IO [()]
largeMersennes = mapM printIfMr (map (\x -> 2^x - 1) primes)
  where printIfMr :: Integer -> IO ()
        printIfMr x = do
          p <- primeMR 1 x
          when p (putStrLn (show x))

{-- Using this method, I found 20 numbers, the largest of which was
285542542228279613901563566102164008326164238644702889199247456602284400390600653875954571505539843239754513915896150297878399377056071435169747221107988791198200988477531339214282772016059009904586686254989084815735422480409022344297588352526004383890632616124076317387416881148592486188361873904175783145696016919574390765598280188599035578448591077683677175520434074287726578006266759615970759521327828555662781678385691581844436444812511562428136742490459363212810180276096088111401003377570363545725120924073646921576797146199387619296560302680261790118132925012323046444438622308877924609373773012481681672424493674474488537770155783006880852648161513067144814790288366664062257274665275787127374649231096375001170901890786263324619578795731425693805073056119677580338084333381987500902968831935913095269821311141322393356490178488728982288156282600813831296143663845945431144043753821542871277745606447858564159213328443580206422714694913091762716447041689678070096773590429808909616750452927258000843500344831628297089902728649981994387647234574276263729694848304750917174186181130688518792748622612293341368928056634384466646326572476167275660839105650528975713899320211121495795311427946254553305387067821067601768750977866100460014602138408448021225053689054793742003095722096732954750721718115531871310231057902608580607
By checking with the Wikipedia entry of known primes, I found out these were the first 20 known Mersenne primes.
--}