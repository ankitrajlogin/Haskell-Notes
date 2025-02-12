-- Difference Between foldr and foldl
-- Both foldr (right fold) and foldl (left fold) are used to reduce a list to a single value by applying a function recursively. However, they differ in the direction in which they process the list and how they accumulate the result.

-- foldr (Right Fold)
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- Processes elements from right to left.
-- Works well with infinite lists (lazy evaluation).
-- Useful when working with cons (:)-based operations, e.g., reconstructing lists.
-- Preserves short-circuiting behavior.
-- Example: Reconstructing a List


-- foldr (:) [] [1, 2, 3]  -- [1, 2, 3]
-- Example: Lazy Evaluation (Can Work with Infinite Lists)


-- take 5 (foldr (:) [] [1..])  -- [1,2,3,4,5]



-- foldl (Left Fold)

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- Processes elements from left to right.
-- Works efficiently with strict evaluation (but can cause stack overflow with large lists).
-- Requires the entire list to be traversed before producing a result.
-- Used when the accumulator should be updated immediately.
-- Example: Sum of a List


-- foldl (+) 0 [1, 2, 3]  -- 6
-- Issue with Lazy Lists


-- foldl (/) 1 [2, 3, 4]  
-- -- (((1 / 2) / 3) / 4) -- Must evaluate completely before returning the final result.
-- This can cause stack overflows for large lists.


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (foldr , foldr)


foldr :: (a->r -> r) -> r -> [a] -> r 
foldr _cons nil [] = nil 
foldr cons nil (x : xs) = cons x (foldr cons nil xs)

foldl :: (r -> a -> r) -> r -> [a] -> r
foldl cons ini = fun ini 
    where 
        fun !acc [] = acc 
        fun !acc (x : xs) = fun (cons acc x) xs 

-- >>> foldl (++) [] [[1, 2], [3, 4], [5]]
-- [1,2,3,4,5]

-- >>> foldr (++) [] [[1, 2], [3, 4], [5]]
-- [1,2,3,4,5]


