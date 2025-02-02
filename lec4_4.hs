{-# LANGUAGE InstanceSigs #-}


module Overloading where

import Prelude hiding ( Eq(..) , Ord(..)) 

-- Eq is a typeclass from the Prelude, which provides the equality operations (==) and (/=) for types that implement it.
-- Eq(..) means:
-- The (..) syntax imports everything associated with Eq—both the typeclass itself and all of its methods ((==) and (/=)).






-- Parametric polymorphism
-- Allows you to use the same implementation in as many contexts as possible.

-- Overloading (ad-hoc polymorphism)
-- Allows you to use the same function name in different contexts, but with different implementations for different types.


-- A type class defines an interface that can be implemented by potential many different types. 

-- Using instance declarations , we can explain how a certain type (or types of a certain shape) implement the interface. 
-- example : 

class Eq a where 
    (==) :: a -> a -> Bool
    x == y = not (x /= y) 
    (/=) :: a -> a -> Bool
    x /=y = not (x == y) 
    {-# MINIMAL (==) | (/=) #-}

-- if we not furthur not define then, there can be a loop 
-- so we need atleast any of them definations. 


instance Eq Bool where
    (==) :: Bool -> Bool -> Bool
    True == True = True 
    False == False = True
    _ == _ = False 


data Choice = Rock | Paper | Scissors 


instance Eq Choice where    
    (==) :: Choice -> Choice -> Bool
    Rock == Rock = True
    Paper == Paper = True
    Scissors == Scissors = True
    _ == _ = False


instance Eq a => Eq [a] where 
    (==) :: [a] -> [a] -> Bool
    [] == [] = True
    (x : xs) == (y : ys) = x == y && xs == ys
    _  == _ = False  



-- ghci> :i Ordering
-- type Ordering :: *
-- data Ordering = LT | EQ | GT
--         -- Defined in ‘GHC.Types’



