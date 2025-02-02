{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (sum , (||)) 

sum :: [Int] -> Int 
sum [] = 0 
sum (x : xs) =  x + sum xs 

-- it is used to ignoring hlint for foldr
{-# ANN module "HLint: ignore Use foldr" #-}



(||) :: Bool -> Bool -> Bool
(||) False y = y
(||) True _y = True 
-- we can write as _ or y or _y
-- all are good 

-- another way to rewrite this : 
-- (||) :: Bool -> Bool -> Bool
-- False || y = y 
-- True || y = True 


-- >>> 2 + 2
-- 4

-- >>> sum [34,234,23,234,22]
-- 547



isAscending :: [Int] -> Bool 
isAscending [] = True 
isAscending [_x] = True 
isAscending (x : y : ys) = x < y && isAscending ys 
