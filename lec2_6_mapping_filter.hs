{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (sum , (||) , length, map , filter , any) 

length :: [a] -> Int
length [] = 0 
length (_ : xs) = 1 + length xs 

addTwo :: Int -> Int
addTwo x = x+2 

map :: (a->b) -> [a]-> [b] 
map _ [] = [] 
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = [] 
filter p (x: xs) = 
    case p x of 
        False -> filter p xs
        True -> x : filter p xs 

-- if p x then x : filter p xs else filter p xs

-- filter p (x : xs) 
-- | p x = x : filter p xs 
-- | otherwise = filter p xs

-- | - we use guards (|) to decide what to do for each element 
-- case 1 : if p x is true then --> x : filter p xs 
-- otherwise - filter p xs 



-- TODO 
-- By pattern matching, redefine the function any :: (a -> Bool) -> [a] -> Bool from the Prelude that checks whether a given list contains at least one element that passes the given test.

any :: (a -> Bool) -> [a] -> Bool 
any _f [] = False
any f (x : xs) = if f x then True else any f xs

-- any f (x : xs) = f x || any f xs