module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

------ | Question 1 (2h) | ------

testF1 = head $ parse "*(1 2)"
testF2 = head $ parse "+(1 2)"
testF3 = head $ parse "*(3 +(4 5))"

testNNF = head $ parse "-*(1 2)"
testArrow = head $ parse "(1 ==> 2)"
testCNF1 = head $ parse "(1 <=> 2)"
testCNF2 = head $ parse "*(1 +(2 *(3 4)))"

-- | logical contradiction 
contradiction :: Form -> Bool
contradiction f = all (\ v -> not $ evl v f) (allVals f)

-- | logical tautology 
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment
entails :: Form -> Form -> Bool
entails f g = all (\ v -> evl v f --> evl v g) (allVals fg)
    where fg = head $ parse $ "*(" ++ show f ++ " " ++ show g ++ ")"

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f


------ | Question 2 (15 min) | ------
checkParse :: [Char] -> Bool
checkParse f = (show $ head $ parse f) == f


------ | Question 3 | ------

{-|
    *Lab3> nnf testNNF
    +(-1 -2)
-}

distributionLaw :: Form -> Form
distributionLaw (Dsj (f1:Cnj (f2:f3:_):_)) = Cnj[ Dsj [f1, f2], Dsj [f1, f3]]
distributionLaw (Dsj (Cnj (f2:f3:_):f1:_)) = Cnj[ Dsj [f1, f2], Dsj [f1, f3]]
distributionLaw (Neg f) = Neg (distributionLaw f)
distributionLaw (Cnj fs) = Cnj (map distributionLaw fs)
distributionLaw (Dsj fs) = Dsj (map distributionLaw fs)
distributionLaw x = x

repetitiveLaw form = if form == new then form
    else repetitiveLaw new
    where new = distributionLaw form

flatten :: Form -> Form
flatten (Cnj (Cnj f1:Cnj f2:f3)) = Cnj(map flatten $ f1++f2++f3)
flatten (Neg f) = Neg (flatten f)
flatten (Cnj fs) = Cnj (map flatten fs)
flatten (Dsj fs) = Dsj (map flatten fs)
flatten x = x

toCNF :: Form -> Form
toCNF = arrowfree # nnf # repetitiveLaw

bla = head $ parse "*(*(+(-1 1) +(-1 2)) *(+(-2 1) +(-2 2)) *(+(-1 1) +(-1 2)) *(+(-2 1) +(-2 2)))"