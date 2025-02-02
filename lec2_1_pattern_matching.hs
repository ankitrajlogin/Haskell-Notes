
{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (length)

import Data.Function 

-- ghci> let x = undefined
-- ghci> x `seq` "This won't print"
-- *** Exception: Prelude.undefined



length :: [a] -> Int
length [] = 0 
length (_ : xs) = 1 + length xs 

-- alternate ways to write this. 


lengthNew :: [a] -> Int
lengthNew [] = 0 
lengthNew (_x : xs) = 1 + lengthNew xs 


-- we can use here _ and _x both as both are same. But it is good to write _x for user redeability. 



sum_list :: [Int] -> Int 
sum_list [] = 0 
sum_list (x : xs) = x + sum_list xs 



----------------------------------------------
--  (&) – Reverse Function Application
----------------------------------------------


result :: Int 
result = 5 & (+1) & (*2)  -- Equivalent to ((5 + 1) * 2)

-- >>> result 
-- 12

-- (&) applies functions from left to right, making it easier to write readable function chains.




