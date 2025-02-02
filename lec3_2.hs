

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Functions where

import Prelude hiding (reverse , sum)



-- simple sum 

-- Num a =>:

-- This is a type class constraint.
-- It specifies that the type a must belong to the Num type class (i.e., it must support numeric operations like addition, subtraction, etc.).
-- This allows sum to work with any numeric type, such as Int, Float, or Double.

sum :: Num a => [a] -> a
sum [] = 0 
sum (x : xs) = x + sum xs 




-- for large list, it can give stack overflow. 

-- ghci> sum [1..1000000000]
-- *** Exception: stack overflow

-- sum [1. .3]
-- = sum (enumFromTo 1 3)
-- = sum (1 : enumFromTo 2 3)|
-- = 1 + sum (enumFromTo 2 3))
-- + sum (2 : enumFromTo 3 3)
-- 12 + sum (enumFromTo 3 3)1
-- 1 + (2 + sum (3 : enumFromTo 4 3)) |
-- + (2 + (3 + sum (enumFromTo 4 3))) |
-- + (2 + (3 + sum [])) |
-- 1 + (2 + (3 + 0))
-- = 1 + (2 + 3)
-- = 6 



-- accumulator idea. 

sumAux2 :: Num a => a -> [a] -> a 
sumAux2 acc [] = acc 
sumAux2 axx (x : xs) = sumAux2 (axx + x) xs


sum2 :: Num a => [a] -> a 
sum2 = sumAux2 0  


-- Still it give stackoverflow. 

-- lets check why. 


-- sum2 [1..3]
-- = sum2 (enumFromTo 1 3)
-- = sumAux2 0 (enumFromTo 1 3)
-- = sumAux2 0 (1 : enumFromTo 2 3)
-- = sumAux2 (0 + 1) (enumFromTo 2 3)
--  = sumAux2 (0 + 1) (2 : enumFromTo 3 3)
-- == SumAux2 ((0 + 1) + 2) (enumFromTo 3 3)
-- = ((0+1) + 2) +3 
-- = (1 + 2) + 3
-- = 3 + 3
-- = 6. 


-- there is not pattern matching for acc but ther is for list. So, still it calculate first list all value. and then finally at the end. it sum all . Hence. there is still stack overflow. 


-- -- Why Use Bang Patterns?
-- Avoid Thunks:

-- Thunks are unevaluated expressions in Haskell that can lead to memory overhead. Bang patterns ensure evaluation happens immediately, reducing memory usage.
-- Optimize Performance:

-- In certain cases, forcing strict evaluation can improve performance by avoiding the buildup of thunks during recursion or data processing.
-- Prevent Space Leaks:

-- By ensuring values are evaluated early, you can avoid retaining references to unevaluated expressions, which can cause space leaks.


sumAux3 :: Num a => a -> [a] -> a
sumAux3 !acc [] = acc 
sumAux3 !acc (x : xs) = sumAux3 (acc + x) xs 

sum3 :: Num a => [a] -> a 
sum3 = sumAux3 0 

-- !acc:
-- The bang pattern (!) ensures strict evaluation of acc. This forces Haskell to evaluate the accumulator immediately at each step rather than deferring computation.
-- This avoids the buildup of a thunk chain (unevaluated expressions),


-- this is how not it work,. 

-- = sum2 (enumFromTo 1 3)
-- = sumAux2 0 (enumFromTo 1 3)
-- = sumAux2 0 (1 : enumFromTo 2 3)
-- = sumAux2 (0 + 1) (enumFromTo 2 3)
-- = sumAux2 1 (enumFromTo 2 3)
--  = sumAux2 1 (2 : enumFromTo 3 3)
-- == SumAux2 (1 + 2) (enumFromTo 3 3)
-- == SumAux2 3 (3 : enumFromTo 3 3)
-- =  SumAux2 (3+3) (enumFromTo 4 3)
-- = SumAux2 6 []
-- = 6 


-- similarly like this, 


reverseAux :: [a] -> [a] -> [a]
reverseAux !acc [] = acc 
reverseAux !acc (x : xs) = reverseAux (x : acc) xs 


reverse_fast :: [a] -> [a] 
reverse_fast  = reverseAux []

-- it does not make difference as acc still calculate hence. space will take. so for this case, it is not necessary. 
