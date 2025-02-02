
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (reverse , sum , elem , map , and , foldr ,zip ,flip)


reverse :: [a] -> [a]
reverse = reverseAux []
    where 
        reverseAux :: [a] -> [a] -> [a] 
        reverseAux !acc [] = acc
        reverseAux !acc (x : xs) = reverseAux (x : acc) xs


sum :: Num a => [a] -> a
sum = sumAux 0
    where 
    sumAux :: Num a => a -> [a] -> a 
    sumAux !acc []= acc
    sumAux !acc (x : xs) = sumAux (acc + x) xs


length :: [a] -> Int
length = lengthAux 0
    where 
        lengthAux :: Int -> [a] -> Int 
        lengthAux !acc [] = acc
        lengthAux !acc (_x : xs) = lengthAux (acc + 1) xs



foldl' :: (r -> a -> r) -> r -> [a] -> r
foldl' upd ini = go ini 
    where 
        go !acc [] = acc 
        go !acc (x : xs) = go (upd acc x) xs 


-- Similarly we can add all the things as well. 

reverse' :: [a] -> [a]
reverse' = foldl' (\ acc x -> x : acc) []


flip :: (a->b->c) -> b->a->c 
flip f b a = f a b 

-- using flip reverse the list . 


reverse'' :: [a] -> [a]
reverse'' = foldl' (flip (:)) []



sum' :: Num a => [a] -> a 
sum' = foldl' (\acc x -> x+ acc) 0 

-- or we can write. 
-- sum = foldl' (+) 0 


length' :: [a] -> Int
length' = foldl' (\acc _ -> acc +1) 0 




-- foldr (right fold):
-- Processes the list from right to left.
-- Starts folding from the last element and works toward the first element.
-- Useful when the operation or function needs to operate on the structure of the list itself (e.g., : to rebuild the list).

-- foldl' (strict left fold):
-- Processes the list from left to right.
-- Starts folding from the first element and works toward the last element.
-- Useful when accumulating values that don’t depend on the structure of the list.


-- foldr:
-- It is lazy. This means it defers computation until it is actually needed.
-- Builds a "thunk" (a deferred computation chain) for every operation and doesn’t immediately reduce the result.
-- Advantage: Works well with infinite lists because it only evaluates as much as required.
-- Disadvantage: For large lists, the chain of thunks can cause a stack overflow due to deferred computations.

-- foldl':
-- It is strict. This means it evaluates the accumulator at every step instead of deferring computation.
-- No thunks are built, so memory usage is more efficient.
-- Advantage: Works well with large lists, avoiding stack overflow.
-- Disadvantage: Cannot process infinite lists, as it tries to consume the entire list immediately.


-- foldr exact working. 

foldr :: (a->r -> r) -> r -> [a] -> r 
foldr _cons nil [] = nil 
foldr cons nil (x : xs) = cons x (foldr cons nil xs) 

-- foldr cons nil [x,y,z]


-- = foldr cons nil (x : (y : (z : [])))
-- = x 'cons' (foldr cons nil (y : z : []))
-- = x 'cons' (y 'cons' (foldr cons nil (z : [])))
-- = x 'cons' (y 'cons' (z 'cons' foldr cons nil []))
-- = x 'cons' (y 'cons' (z 'cons' nil))



foldl'' :: (r -> a -> r) -> r -> [a] -> r
foldl'' upd ini = go ini 
    where 
        go !acc [] = acc 
        go !acc (x : xs) = go (upd acc x) xs 


--  foldl' upd ini [x,y,z]
-- = foldl' upd ini (x : (y : (z: [])))
-- = go ini (x : (y : (z: [])))
-- = go (ini 'upd' x) (y : z : [])
-- = go ((int 'upd' x) 'upd' y) (z : [])
-- = go (((ini 'upd' x) 'upd' y) 'upd' z) []
-- = (((ini 'upd' x) 'upd' y) 'upd' z)


