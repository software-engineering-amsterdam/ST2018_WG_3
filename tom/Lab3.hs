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


-- All valuations that satisfy the formula.
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
-- Is equivalent to entailment in both ways.
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1


{-- Assignment 2 (45m) --}

-- genForm generates a random formula to be used in testing.
pick :: Int -> IO Int
pick n = getStdRandom (randomR (0,n))
genForm :: IO Form
genForm = do
    c <- pick 8  -- A number to determine the type of node.
    x <- pick 5  -- An x for when the node is a proposition.
    f1 <- if c > 3 then genForm else return (Prop 0) -- A child node (only generated if needed)
    f2 <- if c > 4 then genForm else return (Prop 0) -- Another child node (only generated if needed)
    return $ case c of
                 c | c <= 3 -> Prop x
                 4          -> Neg f1
                 5          -> Cnj [f1, f2]
                 6          -> Dsj [f1, f2]
                 7          -> Impl f1 f2
                 8          -> Equiv f1 f2

-- Sometimes formulas are too complex to transform within reasonable time, so
-- with this function we can limit the AST depth.
limitFormDepth :: Int -> Form -> Form
limitFormDepth limit form = lfd' 0 form
    where lfd' :: Int -> Form -> Form
          lfd' depth (Prop x) = Prop x
          lfd' depth (Neg f) | depth < limit = Neg (lfd' (depth+1) f)
                             | otherwise     = Prop 1
          lfd' depth (Cnj xs) | depth < limit = Cnj (map (lfd' (depth+1)) xs)
                              | otherwise     = Prop 2
          lfd' depth (Dsj xs) | depth < limit = Dsj (map (lfd' (depth+1)) xs)
                              | otherwise     = Prop 3
          lfd' depth (Impl f g) | depth < limit = Impl (lfd' (depth+1) f) (lfd' (depth+1) g)
                                | otherwise     = Prop 4
          lfd' depth (Equiv f g) | depth < limit = Equiv (lfd' (depth+1) f) (lfd' (depth+1) g)
                                 | otherwise     = Prop 5
                 
-- Performs a single test of the parse function
testParse :: IO Bool
testParse = do
    form <- genForm
    str <- return $ show form
    return $ form `elem` parse str
 
-- Runs a test with n different inputs.
runTest :: IO Bool -> Integer -> IO Bool
runTest _ 0 = return True
runTest f n = do
    result <- f
    recurse <- runTest f (n-1)
    return $ result && recurse

-- Tests the parse test function for 1000 different inputs.
runTestParse :: IO Bool
runTestParse = runTest testParse 1000


{-- Assignment 3 (1.5h) --}

-- Applies the distributive law
distLaw :: Form -> Form
distLaw (Dsj (p:(Cnj (q:r:_)):_)) = Cnj [Dsj [p, q], Dsj [p, r]]
distLaw (Dsj ((Cnj (q:r:_)):p:_)) = Cnj [Dsj [p, q], Dsj [p, r]]
distLaw (Neg f) = Neg (distLaw f)
distLaw (Cnj fs) = Cnj (map distLaw fs)
distLaw (Dsj fs) = Dsj (map distLaw fs)
distLaw x = x

-- Flattens conjunctions and disjunctions, so (Dsj [Dsj [...], ...]) becomes (Dsj [......])
isDsj, isCnj :: Form -> Bool
isDsj (Dsj _) = True
isDsj _ = False
isCnj (Cnj _) = True
isCnj _ = False
flatten :: Form -> Form
flatten (Dsj xs) = Dsj (map flatten (filter (not . isDsj) xs
                           ++ (concat disjunctions)))
    where unwrap (Dsj x) = x
          disjunctions = (map unwrap (filter (isDsj) xs))
flatten (Cnj xs) = Cnj (map flatten (filter (not . isCnj) xs
                           ++ (concat conjunctions)))
    where unwrap (Cnj x) = x
          conjunctions = (map unwrap (filter (isCnj) xs))
flatten (Neg f) = Neg (flatten f)
flatten f = f

-- Applies the distributive law until the formula does not change anymore.
repeatedLaw :: (Form -> Form) -> Form -> Form
repeatedLaw law f = let applied = law f in
    if law applied == applied
        then applied
        else repeatedLaw law applied
repeatedDistLaw, repeatedFlatten :: Form -> Form
repeatedDistLaw = repeatedLaw distLaw
repeatedFlatten = repeatedLaw flatten

toCNF :: Form -> Form
toCNF = arrowfree # nnf # repeatedDistLaw # repeatedFlatten

-- Checks if something is a CNF by checking if it consists of clauses
isCNF :: Form -> Bool
isCNF (Cnj xs) = foldr (\x r -> isClause x && r) True xs
isCNF x = isClause x
-- Checks if something is a clause by checking if it consists of literals.
isClause (Dsj xs) = foldr (\x r -> isLiteral x && r) True xs
isClause x = isLiteral x
-- Checks if something is a literal by checking if it is a proposition,
-- optionally embedded in a negation.
isLiteral (Prop _) = True
isLiteral (Neg x) = isLiteral x
isLiteral _ = False

-- Test the toCNF function
testToCNF :: IO Bool
testToCNF = do
    fullForm <- genForm
    form <- return $ limitFormDepth 5 fullForm
    putStrLn "--------------------------"
    putStrLn ("Testing formula: " ++ show form)
    cnf <- return $ toCNF form
    putStrLn ("CNF: " ++ (show cnf))
    result <- return $ isCNF cnf && equiv form cnf
    putStrLn ("Result: " ++ show result)
    return $ result

runTestToCNF :: IO ()
runTestToCNF = do
    result <- runTest testToCNF 100
    putStrLn "--------------------------"
    putStrLn ("Combined test result: " ++ show result)
    

{-- Assignment 4 (0h) --}

{--

Basically I already did this in the previous exercises as part of a TDD workflow.
The runTestToCNF function performs 100 tests of the toCNF function, where it checks
whether the resulting formula is in CNF and whether it is equivalent to the input
formula. When running this test function, the overall test result is True, meaning
that all 100 of the tests have succeeded.

--}