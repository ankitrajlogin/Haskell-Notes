{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude 



-- >>> :t drop 
-- drop :: Int -> [a] -> [a]


-- >>> drop 3 [1,2,3,4,5,5,4,3]
-- [4,5,5,4,3]

-- drop will drop first n element and then return rest. 


-- >>> :t take 
-- take :: Int -> [a] -> [a]


-- >>> take 3 [1,2,3,4,5,5,3]
-- [1,2,3]

-- take will take first n element and then return



sliceF :: Int -> Int -> [a] -> [a] 
sliceF i l xs = if i+l > length xs || i < 0 || l <= 0 then [] else 
    take l ( drop i xs) 


slice2 :: Int -> Int -> [a] -> [a]
slice2 i l xs 
    | i < 0 || l <= 0 = [] 
    | i > length xs = [] 
    | otherwise = take l (drop i xs) 


-- >>> slice2 3 4 [1,2,3,4,5,6,7,8,9]
-- [4,5,6,7]




-- Define a function snoc :: [a] -> a -> [a] that appends a single element to the end of a list, in two different ways: (1) as a one-liner reusing (++), and (2) without reusing (++), by using pattern matching.


snocF :: [a] -> a -> [a]
snocF xs val = xs ++ [val] 

snoc :: [a] -> a -> [a]
snoc [] val = [val]  
snoc (x : xs) val = x : snoc xs val  


snocV :: [a] -> a -> [a] 
snocV xs val = foldr (:) [val] xs 







-- Define a function nub :: Eq a => [a] -> [a] that removes all duplicates from a list. Do this by pattern matching and by calling filter in every recursive step. (Quadratic behaviour is expected here, and necessary as long as we do not impose further constraints than just Eq.) This function is available not from the Prelude, but from the Data.List module.

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x)  xs) 







