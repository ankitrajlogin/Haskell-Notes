{-# LANGUAGE InstanceSigs #-}


module Overloading where

import Prelude hiding ( Eq(..) , Ord(..)) 



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






-------------- IMPORTANT CLASSES ---------------


--- Starting from here. 


class Eq a => Ord a where 
    (<=) :: a -> a -> Bool 
    (<) :: a-> a -> Bool
    x < y = x <= y && not ( x == y) 


instance Ord Bool where 
    False <= y = True
    True <= y = y 



instance Eq a => Eq (Maybe a) where 



instance Ord a => Ord (Maybe a) where 



-- normal gchi output. 
-- ghci> [9] <[9, 1]
-- True

-- ghci> [0] < [0,1]
-- True

-- ghci> "bar" < "baz"
-- True


--------------Enum -----------------------

-- ghci> enumFromTo 1 10
-- [1,2,3,4,5,6,7,8,9,10]

-- ghci> enumFromTo 'a' 'z'
-- "abcdefghijklmnopqrstuvwxyz"

-- ghci> :i Enum
-- type Enum :: * -> Constraint
-- class Enum a where
--   succ :: a -> a
--   pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
--   enumFrom :: a -> [a]
--   enumFromThen :: a -> a -> [a]
--   enumFromTo :: a -> a -> [a]
--   enumFromThenTo :: a -> a -> a -> [a]
--   {-# MINIMAL toEnum, fromEnum #-}


-- example : 

-- ghci> [1.0 .. 4.5]
-- [1.0,2.0,3.0,4.0,5.0]

-- ghci> [1.2 .. 4.5]
-- [1.2,2.2,3.2,4.2]

-- ghci> [1,5..100]
-- [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81,85,89,93,97]





------- Bounded Class -----------------------

-- ghci> :i Bounded
-- type Bounded :: * -> Constraint
-- class Bounded a where
--   minBound :: a
--   maxBound :: a
--   {-# MINIMAL minBound, maxBound #-}


-- ghci> minBound
-- ()

-- ghci> maxBound
-- ()

-- ghci> minBound :: Int
-- -9223372036854775808

-- ghci> minBound :: Char
-- '\NUL'

-- ghci> maxBound :: Char
-- '\1114111'

-- ghci> minBound :: (Char , Bool , Int)
-- ('\NUL',False,-9223372036854775808)
