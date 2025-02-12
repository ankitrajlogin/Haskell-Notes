{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (reverse ,  elem  , and , foldr  ,flip , (.) , id , ($), and ) 


sumEvenSquares :: [Int] -> Int
sumEvenSquares xs = sum (map (\x -> x*x) (filter even xs))


-- sumEvenSquares' :: [Int] -> Int
-- sumEvenSquares' [] = 0 
-- sumEvenSquares' (x : xs) 
--     | even x = x*x + sumEvenSquares' xs 
--     | otherwise = sumEvenSquares' xs 

(.) :: (b->c) -> (a->b) -> a->c 
(f . g) x  =  f (g x) 
-- or f . g = \x -> f (g x) 


-- so we can write sumEvenSquares function using . operator. 

sumEvenSquares' :: [Int] -> Int
sumEvenSquares' = sum . map (\x -> x*x) . filter even
-- sumEvenSquares' xs = (sum . map (\x -> x*x) . filter even) xs 


countLongerThanFive :: [[a]] -> Int
countLongerThanFive xs = length (filter (\x -> length x > 5) xs)

-- countLongerThanFive xs = length . filter (\x -> length x >=5) xs
-- countLongerThanFive xs = length . filter (\x -> (\y -> y>=5) (length x)) xs

-- countLongerThanFive xs = length . filter (\x -> (>= 5) (length x)) xs

-- countLongerThanFive xs = length . filter ((>=5) . length)  xs



-- Understanding the syntax (\x -> length x > 5) 

-- \x -> defines a lambda function that takes one argument x.
-- length x > 5 is the body of the function, which:
-- Computes length x (the number of elements in x).
-- Compares it to 5 (> 5), producing a Boolean value (True or False).






-- Identity function ---------------------
-------------------------------------------

-- ghci> :t id
-- id :: a -> a

id :: a -> a 
id x = x














-- ghci> :t all 
-- all :: Foldable t => (a -> Bool) -> t a -> Bool

and :: [Bool] -> Bool 
and = all id 



-----------------------------------------
-- Application operator  ($) 
-----------------------------------------

($) :: (a->b) -> a->b
f $ x = f x 

-- Why Use ($)?
-- 1. Eliminating Parentheses
-- Compare these two expressions:

-- Without ($) (lots of parentheses):

-- print (sum (map (*2) [1,2,3,4]))

-- print $ sum $ map (*2) [1,2,3,4]


-- >>> sum $ map (*2) [1,2,3,4]
-- 20




-- ghci> :t map
-- map :: (a -> b) -> [a] -> [b]


-- ghci> :t map id 
-- map id :: [b] -> [b]




-- all function ---------------------
-------------------------------------------

-- ghci> :t all
-- all :: Foldable t => (a -> Bool) -> t a -> Bool


-- ghci> all even [3,43,23,53,2342]
-- False

-- ghci> all even [2,4,6,8]
-- True


-- lIst of  function ---------------------
-------------------------------------------
listOfFunctions :: [Int -> Int]
listOfFunctions = [(+1) , (*2) , (+7) , (*4)]

-- ghci> map (\f -> f 3) listOfFunctions
-- [4,6,10,12]


-- we can also use `id` for same. 


-- ghci> map ($ 3) listOfFunctions
-- [4,6,10,12]

-- so we can use $ as well for . operator. 

-- sumEvenSquares' = sum . map (\x -> x*x) . filter even
-- sumEvenSquares' = sum $ map (\x -> x*x) $ filter even

