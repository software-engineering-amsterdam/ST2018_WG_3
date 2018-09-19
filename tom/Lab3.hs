module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck

import Lecture3

-- Test forms
testF1 = head $ parse "*(1 +(2 -3))"
testF2 = head $ parse "-(5==>*(5 4))"
testF3 = head $ parse "*(5 -5)"
testF4 = head $ parse "+(5 -5)"
testE1 = head $ parse "(1==>2)"
testE2 = head $ parse "+(-1 2)"

{-- Assignment 1 (1.5h) --}

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f) -- Like satisfiable but with all instead of any



satValuations :: Form -> [Valuation]
satValuations f = filter (\v -> evl v f) (allVals f)

-- Whether xs is a sublist of ys
isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] ys = True
isSublist xs [] = False
isSublist (x:xs) ys | x `elem` ys = isSublist xs (delete x ys)
                    | otherwise   = False

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = satValuations f1 `isSublist` satValuations f2

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1


{-- Assignment 2 --}

pick :: Int -> IO Int
pick n = getStdRandom (randomR (0,n))
genForm :: IO Form
genForm = do
    c <- pick 10
    x <- pick 5
    f1 <- if c > 5 then genForm else return (Prop 0)
    f2 <- if c > 6 then genForm else return (Prop 0)
    return $ case c of
                 c | c <= 5 -> Prop x
                 6          -> Neg f1
                 7          -> Cnj [f1, f2]
                 8          -> Dsj [f1, f2]
                 9          -> Impl f1 f2
                 10         -> Equiv f1 f2
                 
testParse :: IO Bool
testParse = do
    form <- genForm
    str <- return $ show form
    return $ form `elem` parse str
                 
runTest :: IO Bool -> Integer -> IO Bool
runTest _ 0 = return True
runTest f n = do
    result <- f
    recurse <- runTest f (n-1)
    return $ result && recurse
                 
runTestParse :: IO Bool
runTestParse = runTest testParse 1000