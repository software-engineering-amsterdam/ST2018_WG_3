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
distributionLaw (Dsj (Prop x Cnj (f2:f3))) = Cnj (Dsj (x f2) Dsj (x f3)) 
distributionLaw (Prop x) = Prop x 
distributionLaw (Neg f) = Neg (distributionLaw f)
distributionLaw (Cnj fs) = Cnj (map distributionLaw fs)
distributionLaw (Dsj fs) = Dsj (map distributionLaw fs)

toCNF :: Form -> Form
toCNF = arrowfree # nnf