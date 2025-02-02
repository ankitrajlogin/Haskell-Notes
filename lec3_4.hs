{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (reverse , sum , elem , map , and , foldr ,zip)


elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = x == y || elem x ys


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x: map f xs


-- assuming empty list is true. 
and :: [Bool] -> Bool
and [] = True
and (x: xs) = x && and xs

-- performance:
-- Best case: O(1) (if the first element is False).
-- Worst case: O(n) (if the list is fully evaluated, e.g., all elements are True).




fun :: (a->r -> r) -> r -> [a] -> r 
fun _cons nil [] = nil 
fun cons nil (x : xs) = cons x (fun cons nil xs) 

-- x: Represents the current element of the list being processed.
-- nil : Represents the accumulated result so far (starting with False for elem').

-- as we define cons taking two argument and resulting one. 



-- Eq a: The type a must belong to the Eq typeclass, meaning elements of type a can be compared for equality (using ==).

elem' :: Eq a => a -> [a] -> Bool
elem' y xs = fun (\x r -> y == x || r) False xs

-- x: The current element of the list being processed.
-- r: The accumulated result so far (initially False).

-- ghci> elem' 10000 [1..]
-- True


map' :: (a->b) -> [a] -> [b]
map' f = fun (\x r -> f x : r) []

and' :: [Bool] -> Bool
and' = fun (\x r -> x && r) True 


-- ghci> map' (+1) [1, 2, 3, 4]
-- [2, 3, 4, 5]

-- ghci> map' (*2) [1, 2, 3, 4]
-- [2, 4, 6, 8]

-- ghci> map' show [1, 2, 3]
-- ["1", "2", "3"]


-- ghci> and' [True, True, True]
-- True

-- ghci> and' [True, False, True]
-- False

-- ghci> and' []
-- True  -- The "and" of an empty list is `True` by definition.



-- This function is called Foldr
-- It's used to reduce or fold a list into a single result by applying a binary function. The "r" in foldr stands for right because it processes the list from right to left.

-- Type Signature of folder 
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- (a -> b -> b): The first argument is a function that takes two arguments:

-- a: The current element of the list.
-- b: The accumulated result so far.
-- It returns a new accumulated result (b).
-- b: The second argument is the initial value (or base case), which serves as the starting value of the accumulator.

-- [a]: The third argument is the list to process.

-- b: The final result, which is a single value of type b (the result of applying the function over the entire list).

foldr :: (a->r -> r) -> r -> [a] -> r 
foldr _cons nil [] = nil 
foldr cons nil (x : xs) = cons x (foldr cons nil xs) 


elem2 :: Eq a => a-> [a] -> Bool
elem2 y = foldr (\x r -> y == x || r) False


map2 :: (a-> b) -> [a] -> [b]
map2 f = foldr (\x r -> f x : r) []


and2 :: [Bool] -> Bool
and2 = foldr (&&) True


-- How It Works:
-- foldr processes the list from right to left. It applies the provided function to the last element of the list and the base case, then uses that result to process the previous element, and so on until the entire list has been reduced to a single value.

-- Here’s the recursive process for how foldr works:

-- If the list is empty ([]), return the base case value (b).
-- If the list is non-empty (x : xs), apply the function to the current element (x) and the result of recursively folding the tail (xs).

sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs


reverseList :: [a] -> [a]
reverseList xs = foldr (\x acc -> acc ++ [x]) [] xs


-- ghci> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b


-- zip' :: [a] -> [b] -> [(a, b)]
-- zip' (y:ys) (x : xs) = foldr (\x acc -> case acc of
--                           (y:ys) -> (x, y) : ys  
--                           [] -> []) []



