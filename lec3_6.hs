{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (reverse ,  elem  , and , foldr  ,flip , (.) , id , ($)) 


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

-- countLongerThanFive = length . filter (\x -> length x >=5) 
-- countLongerThanFive = length . filter (\x -> (\y -> y>=5) (length x))

-- countLongerThanFive = length . filter (\x -> (>= 5) (length x))

-- countLongerThanFive = length . filter ((>=5) . length) 



-- Identity function ---------------------
-------------------------------------------

-- ghci> :t id
-- id :: a -> a

id :: a -> a 
id x = x


($) :: (a->b) -> a->b
f $ x = f x 


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