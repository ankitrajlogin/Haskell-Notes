{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (zip , zipWith)


-- Input:
-- A list of elements of type a ([a]).
-- A list of elements of type b ([b]).

-- Output:
-- A list of pairs ([(a, b)]), where each pair contains one element from the first list and one from the second.

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = [] 
zip _ [] = [] 
zip (x : xs) (y : ys) = (x,y) : zip xs ys



-- code fo zipWith
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _f [] _ = [] 
zipWith _f _ [] = [] 
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys 

-- output
-- ghci> zipWith (:) "Hw" ["ello" , "orld"]
-- ["Hello","world"]

-- ghci> zipWith (\x y -> x + y) [1,3,4,5,4][3,5,56,3,43]
-- [4,8,60,8,47]

-- ghci> zipWith (+)  [1,3,4,5,4][3,5,56,3,43]
-- [4,8,60,8,47]




-- we can apply zip with zipWith
zip' :: [a] -> [b] -> [(a,b)] 
zip' a b = zipWith (,) a b


-- redefine the zipWith
-- This handles all cases where at least one of the input lists is empty.
-- Since zipWith' cannot continue pairing elements if one list is exhausted, it returns an empty list.

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys 
zipWith' _ _ _ = [] 


delete :: Eq key => key -> [(key , val)] -> [(key , val)]
delete _ [] = [] 
delete key ((x,y) : xs) = if key == x then delete key xs else (x,y) : delete key xs 

