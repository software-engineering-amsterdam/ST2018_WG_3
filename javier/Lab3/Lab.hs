module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{--Lecture extras--}


{-- Exercise 1 - 1h30min --}
p', q' :: Form
p' = Prop 1
q' = Prop 2

tautology, contradiction :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)
contradiction f = not (satisfiable f)

testPropContradiction, testPropTautology :: Form
testPropTautology = Equiv p' p'
testPropContradiction = Equiv p' (Neg p')

doTestTautology1, doTestTautology2, doTestContradiction1, doTestContradiction2 :: Bool
doTestTautology1 = tautology testPropTautology
-- true
doTestTautology2 = tautology testPropContradiction
-- False
doTestContradiction1 = contradiction testPropContradiction
-- true
doTestContradiction2 = contradiction testPropTautology
-- False

-- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = all (\ v -> (evl v f1) --> (evl v f2)) (allVals f1)

testEntails1, testEntails2 :: Form
testEntails1 = Cnj [p',q']
testEntails2 = Dsj [p',q']

doTestEntails1, doTestEntails2 :: Bool
doTestEntails1 = entails testEntails1 testEntails2
-- true
doTestEntails2 = entails testEntails2 testEntails1
-- False

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = all
  (\v -> ((evl v f1) --> (evl v f2)) && ((evl v f2) --> (evl v f1))) (allVals f1)

testEquiv1, testEquiv2 :: Form
testEquiv1 = Equiv p' p'
testEquiv2 = Neg (Equiv p' p')

doTestEquiv1, doTestEquiv2 :: Bool
doTestEquiv1 = equiv testEquiv1 (Neg testEquiv2)
--True
doTestEquiv2 = equiv testEquiv1 testEquiv2
--False


--Exercise 2 Test parse 2H30
randomNumber :: Int -> IO Int
randomNumber n = getStdRandom (randomR (1,n))

generateFormula :: IO Form
generateFormula = do
  t <- randomNumber 6
  n1 <- randomNumber 6
  n2 <- randomNumber 6
  c <- randomNumber 3
  f1 <- if (c==1) then generateFormula else return (Prop n1)
  f2 <- if (c==1) then generateFormula else return (Prop n2)
  return $ case t of
    1 -> Prop n1
    2 -> Neg f1
    3 -> Cnj [f1,f2]
    4 -> Dsj [f1,f2]
    5 -> Impl f1 f2
    6 -> Equiv f1 f2

testParse :: IO Bool
testParse = do
  formula <- generateFormula
  formulaString <- return $ show formula
  return (formula == head (parse formulaString))

doTestParse :: Int -> IO String
doTestParse n = do
  result <- testParse
  if (n == 0) then return "All tests passed"
    else if (result == False) then return "Error!"
      else doTestParse (n-1)

runTestParse :: Int -> IO String
runTestParse n = doTestParse n

-- runTestParse 100 gives all test Passed


-- Exercise 3 - Formulas into CNF 6h
removeArrowsAndNegation :: Form -> Form
removeArrowsAndNegation f = nnf $ arrowfree f

doRemoveArrowsAndNegation = do
  formula <- generateFormula
  return (removeArrowsAndNegation formula)

--Step 1, remove all starts (pattern -> +(*)) until one remains = Distribution
applyDistribution :: Form -> Form
applyDistribution (Dsj(p:(Cnj(q:r:_)):_)) = applyDistribution (Cnj[Dsj[p,q], Dsj[p,r]])
applyDistribution (Dsj((Cnj(q:r:_)):p:_)) = applyDistribution (Cnj[Dsj[p,q], Dsj[p,r]])
applyDistribution (Cnj fs) = Cnj (map applyDistribution fs)
applyDistribution (Dsj fs) = Dsj (map applyDistribution fs)
applyDistribution f = f

--Step 2, remove parethensis (because +(+(6 6) -4) == +(6 6 -4)
removeParenthesis :: Form -> Form
removeParenthesis (Dsj(Dsj(p:q:_):Dsj(r:s:_):_)) =
                                              removeParenthesis(Dsj[p,q,r,s])
removeParenthesis (Cnj(Cnj(p:q:_):Cnj(r:s:_):_)) =
                                              removeParenthesis(Cnj[p,q,r,s])
removeParenthesis (Cnj(Dsj(p:q:_):Dsj(r:s:_):x:_)) =
                                              removeParenthesis(Cnj[Dsj[p,q,r,s],x])
removeParenthesis (Cnj(x:Dsj(p:q:_):Dsj(r:s:_):_)) =
                                              removeParenthesis(Cnj[x,Dsj[p,q,r,s]])
removeParenthesis (Dsj(Cnj(p:q:_):Cnj(r:s:_):x:_)) =
                                              removeParenthesis(Dsj[Cnj[p,q,r,s],x])
removeParenthesis (Dsj(x:Cnj(p:q:_):Cnj(r:s:_):_)) =
                                              removeParenthesis(Dsj[x,Cnj[p,q,r,s]])
removeParenthesis f = f

--Gives back a CNF formula
doCnf = do
  formula <- generateFormula
  return (removeParenthesis(applyDistribution (removeArrowsAndNegation formula)))


--Exercise 4 Formula Generator for random testing
