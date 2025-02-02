
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Functions where

import Prelude hiding (reverse , sum , elem , map)



elem :: Eq a => a -> [a] -> Bool
elem _ [] = False 
elem x (y : ys) = x == y || elem x ys 

-- ghci> elem 0 [1..10000]
-- False
-- ghci> elem 1000 [1..1000000]
-- True


-- Elem function using accumulator. 


elemAux :: Eq a => Bool -> a -> [a] -> Bool
elemAux !acc _ [] = acc 
elemAux !acc x (y : ys) = elemAux (acc || x == y) x ys 

elem' :: Eq a => a -> [a] -> Bool
elem' = elemAux False 

-- Time Complexity: 
-- 𝑂(𝑛) where n is the length of the list.
-- The function always traverses the entire list, regardless of whether the element is found early or not.



elemAux2 :: Eq a => Bool -> a -> [a] -> Bool
elemAux2 True _ _ = True 
elemAux2 !acc _ [] = acc 
elemAux2 !acc x (y : ys) = elemAux2 (acc || x == y) x ys 

elem2' :: Eq a => a -> [a] -> Bool
elem2' = elemAux2 False


-- Second Version:
-- Time Complexity: Best-case 
-- O(1) (if the element is found in the first position).

-- Worst-case 
-- O(n) (if the element is not found or is at the end of the list).

-- This version stops as soon as the target element is found, saving time in many cases.


map :: (a->b) -> [a] -> [b]
map _ [] = [] 
map f (x : xs) = f x : map f xs 



-- When to Use an Accumulator
-- Performance Optimization: Use it when you are working with large inputs and need to avoid stack overflow.
-- Tail-Recursion: When you want the function to be tail-recursive for better memory efficiency.
-- Intermediate State: When intermediate results (like running totals or concatenations) need to be carried forward through the recursive calls.



-- When to Use BangPatterns
-- 1. Avoiding Thunk Buildup
-- A thunk is a deferred computation created by Haskell’s lazy evaluation. Thunks consume memory and can lead to performance degradation if they pile up too much before being evaluated.
-- 2. Preventing Space Leaks
-- Space leaks happen when memory usage grows unnecessarily due to unevaluated thunks. Using BangPatterns ensures that values are evaluated as soon as possible, avoiding these leaks.