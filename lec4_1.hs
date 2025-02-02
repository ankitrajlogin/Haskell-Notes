
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (reverse ,  elem  , and , foldr  ,flip , (.) , id , ($) , fst) 


fst :: (a, b) -> a  -- best to use this.
fst (x, _y) = x 

-- all possible type of signatures :
-- fst :: (Int , Bool) -> Int


-- Haskell terms carry no type information at run-time. 




--- NOTE -------------------------
----------------------------------

    
-- restrictedFst :: (Int, Int) -> Int
-- restrictedFst = fst --ok


-- newFst :: (a, b) -> as
-- newFst = restrictedFst -- type error!


-- restrictedFst expects both components of the tuple to be of type Int. However, newFst implies that the tuple can contain any types a and b. These requirements are incompatible.
-- In other words, Haskell does not automatically "generalize" a concrete type like (Int, Int) to a polymorphic one like (a, b).





-- number of function of this type 
-- (Int , Int) -> (Int, Int)

f1 :: (Int , Int) -> (Int , Int)
f1 (x , y) = (x+y , x*y) 

f2 :: (Int , Int) -> (Int , Int) 
f2 (x , y) = (345 , x*x + y - 17) 

f3 :: (Int , Int) -> (Int , Int)
f3 _ = (0 , 1) 


f4 :: (Int , Int) -> (Int , Int) 
f4 (x  , _) = (x `div` 5 , x `div` 7) 



-- number of function of this type. 
-- (a , a) -> (a,a) 

-- g1 :: (a , a) -> (a , a) 
-- g1 (x  , y) = (x + y , x*y) 

-- The error occurs because the (+) and (*) operators are specific to types that are instances of the Num typeclass. In your function g1, the type variable a is completely unconstrained, meaning it could be any type, but (+) and (*) are only defined for numeric types.


-- g2 :: (a , a) -> (Int , Int) 
-- g2 (x , y) = ( x, y) 

-- we can not write this as well as a can be any type. 


g3 :: (a , a ) -> (Int , Int)
g3 (_x , _y) = ( 1, 4) 

-- this is correct as x and y can be any type and output is Integer. As there is not instance of a is output. 



-- g4 :: (a ,a ) -> (a , a) 
-- g4 (x , y) = if x == y then (x , y) else (y, x) 

--  The comparison x == y requires that the type a is an instance of the Eq typeclass because (==) is defined in the Eq typeclass. Without an Eq constraint, Haskell doesn't know if a
    



g5 :: Eq a => (a , a) -> (a , a) 
g5 (x , y) = if x == y then ( x, x) else (y , y) 

-- as we define a is an instance of the Eq typeclass. 




-- (a ,b ) --> (b , a) 


h1 :: (a , b) -> (b , a) 
h1 (x , y) = (y , x) 


-- h2 :: (a, b) -> (b , a) 
-- h2 (x , y) = ( 1 , 3) 

-- The type signature specifies that the output must have the form (b, a) where:

-- The first element (b) must match the type of the second element of the input tuple (y).
-- The second element (a) must match the type of the first element of the input tuple (x).
-- However, (1, 3) does not depend on the types of x and y from the input tuple at all. It always returns (Int, Int) regardless of the input types.





