module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--satisfiable :: Form -> Bool
--satisfiable f = any (\ v -> evl v f) (allVals f)

--Assingment 1 (3.30h)
contradiction :: Form -> Bool
contradiction f = not (satisfiable f) 

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)


-- | logical entailment 
entails :: Form -> Form -> Bool
entails f0 f1 = all (\ v -> evl v f0 --> evl v f1) (allVals f0) 

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f0 f1 = entails f0 f1 && entails f1 f0


--Assingment 2 (5h) 

{-I will try to pass an logical expression lets name it (1), to function show and then I will pass it to
parser function and will out lets say (2). So if (1) == (2) the test will be successful
-}


workShow :: [Char] -> [Char]
workShow m = show m --give an expression

workParse :: [Char] -> Bool
workParse g =  head (parse workShow g) == g

{-
checkIt :: [Char] -> Bool
checkIt l = 
-}
{-
checkIt :: [Char] -> IO String
checkIt l = do
            expInit <- workShow l
            expLat <- workParse (expInit)
            if (expLat == l) then return "True" else return "False"
			
-}






