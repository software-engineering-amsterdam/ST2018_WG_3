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


{-- Assignment 3 --}

{-- 

See Lecture6.hs.

Implementation is like this:

composites :: [Integer]
composites = filter (not.prime) [2..]

--}


{-- Assignment 4 --}

foolsCheck :: Integer -> Integer -> IO Bool
foolsCheck k x = do
  r <- primeTestsF (fromIntegral k) x
  return $ r /= prime x

firstFool :: Integer -> IO Integer
firstFool k = do
  r <- findM (foolsCheck k) [3..]
  return $ fromJust r

