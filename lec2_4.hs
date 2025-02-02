

{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (product)


product :: [Int] -> Int 
product [] = 0
product [el1] = el1  
product (x : xs) = x* product xs 

-- alternate 
productNew :: [Int] -> Int 
productNew [] = 1
productNew (ele : array) = ele * productNew array



