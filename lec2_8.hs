
{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (drop , take , filter , (++) , reverse) 

-- DROP and TAKE

-- ghci> drop 3 [1..10]
-- [4,5,6,7,8,9,10]


-- we can't write 
-- drop :: Int -> [a] -> [a]  as we want to use == operator.Applicative

-- Added Eq a constraint:

-- Since == is used to compare f and x, we must ensure that a is an instance of the Eq typeclass.


-- DropSame remove just first occurence of the element 

dropSame :: Eq a => a -> [a] -> [a]
dropSame _f [] = [] 
dropSame f (x : xs) = if f == x then xs else x : dropSame f xs




-- drop function drop the first n element from the list. 

drop :: Int -> [a] -> [a]
drop _i [] = [] 
drop i (x : xs) = if i<=0 then x : xs else drop (i-1) xs

-- or this also work fine. 

-- drop :: Int -> [a] -> [a] 
-- drop _i [] = [] 
-- drop i (x : xs)
--     | i <= 0 = x : xs 
--     | otherwise = drop (i-1) xs 

filter :: (a->Bool) -> [a] -> [a]
filter _ [] = [] 
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs 



-- it is used to ignoring hlint for foldr
{-# ANN module "HLint: ignore Use foldr" #-}

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys 



-- (++) :: [a] -> [a] -> [a]


(++) :: [a] -> [a] -> [a]
[] ++ ys = ys  
(x : xs) ++ ys = x : xs ++ ys 


reverse :: [a] -> [a]
reverse [] = [] 
reverse (x : xs) = reverse xs ++ [x]



take :: Int -> [a] -> [a]
take _ [] = [] 
take n (x : xs) = if n <= 0 then [] else x : (take (n-1) xs) 

-- >>> take 4 [1,2,3,4,56,7,6,5]
-- [1,2,3,4]


