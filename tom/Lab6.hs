module Lab6 where
  
import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck hiding (forAll)
import Lecture6


infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q



{-- Assignment 1 --}

{--

See Lecture6.hs.

Implementation is like this:
exM :: Integer -> Integer -> Integer -> Integer
exM x 0 n = rem 1 n
exM x e n = rem (x * exM x (e-1) n) n

--}

-- Testing the function
testExM :: Integer -> Integer -> Integer -> Bool
testExM x e n = n > 0 && e >= 0 --> exM x e n == expM x e n


{-- Assignment 2 --}

