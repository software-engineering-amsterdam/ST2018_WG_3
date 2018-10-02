module Lab4 
where
 
import Data.List
import System.Random
import Test.QuickCheck  
import SetOrd

data Color = W | B deriving (Eq,Show)

drawPebble :: [Color] -> [Color]
drawPebble [] = []
drawPebble [x] = [x]
drawPebble (W:W:xs) = drawPebble (B:xs) 
drawPebble (B:B:xs) = drawPebble (B:xs) 
drawPebble (W:B:xs) = drawPebble (W:xs) 
drawPebble (B:W:xs) = drawPebble (W:xs) 

drawPebbleList xs = (drawPebble xs, xs)

parityDrawPebbleList xs = (parityW (drawPebble xs), drawPebble xs, xs)

instance Arbitrary Color where
  arbitrary = oneof [return W, return B]

numberW :: [Color] -> Int
numberW = length . (filter (== W)) 
    
parityW :: [Color] -> Int
parityW xs =  mod (numberW xs) 2
    
prop_invariant xs = 
  parityW xs == parityW (drawPebble xs)

prop_length xs = length xs == length (drawPebble xs)

sampleF f g =
 do cases <- sample' g
    sequence_ (map (print.f) cases)


--Assignment 5 (1.30h)--

type Rel a = [(a,a)]
{- rev reverses the elements of a tuple (ex. rev (1,2) = (2,1)). 
nub removes duplicates (used from lab's html)
maps every tuple of the list a with the rev function and adds it on the list of tuples, a
-}
symClos :: Ord a => Rel a -> Rel a
symClos a =nub (a ++ map (rev) a) 
     where rev (first, second) = (second, first)



--Assignment 2()--
--instance Arbitrary (Set a) where
  --arbitrary = fmap Set [choose (1,10),choose (1,10)]
  --arbitrary = Set [(choose(1,10),choose(1,10))]

{-
instance Arbitrary (Set a) where
  arbitrary = do
    ws <- [choose(1,10), choose(1,10)]
    return $ Set ws

-}
  
--Assignment 6--  
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 




{-     
instance Arbitrary (Set a) where
  arbitrary = fmap Set [[choose (1,10)],[choose (1,10)]]
-}


  {-
--Assingment 2--
--numGen :: Int -> IO Int
numGen :: IO Int
numGen = getStdRandom (randomR (1,100))


genSets 
-}

{-
genSets n =
  do
    x <- numGen
    ns <- ns : x
	--genSets (n-1)
	if (n == 0) then return ns else genSets(n-1)
-}
{-genSets :: Int -> Int -> [Int]
genSets n x 
   | n == 0   = show x
   | otherwise = x : numGen (n-1)
   -}
   
   
   
   
   
   