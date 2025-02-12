{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (fst , uncurry) 



-- left side -> constructor pair 
-- right side -> type pair 

data Pair a b = Pair a b
    deriving Show 

-- >>> Pair 'x' True 
-- Pair 'x' True

-- or 

-- data Pair a b = MkPair a b
--     deriving Show

-- Testing
-- ghci> (\ (Pair x _) -> x) (Pair 'x' True)
-- 'x'


-- Information about pair 
-- ghci> :i (,)
-- type (,) :: * -> * -> *
-- data (,) a b = (,) a b
--        -- Defined in ‘GHC.Tuple’
-- instance (Monoid a, Monoid b) => Monoid (a, b)
--   -- Defined in ‘GHC.Base’
-- instance (Semigroup a, Semigroup b) => Semigroup (a, b)
--   -- Defined in ‘GHC.Base’
-- instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
-- instance Traversable ((,) a) -- Defined in ‘Data.Traversable’
-- instance (Bounded a, Bounded b) => Bounded (a, b)
--   -- Defined in ‘GHC.Enum’
-- instance (Read a, Read b) => Read (a, b) -- Defined in ‘GHC.Read’
-- instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b) => Ord (a, b) -- Defined in ‘GHC.Classes’
-- instance (Show a, Show b) => Show (a, b) -- Defined in ‘GHC.Show’
-- instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’










-- What is FSt
-- It takes a pair (a, b) as input.
-- It returns the first element of the pair (a)

fst :: Pair a b -> a
fst (Pair x _) = x


fst' :: (a,b) -> a
fst' (x, _) = x 


-- here we passing pair of interger , and in return , we getting sum of that. 
plus :: (Int , Int) -> Int
plus (x, y) = x + y

-- here we passing directly two integer , and in return , we getting sum of that. 
plus' :: Int -> Int -> Int 
plus' x y = x + y


-- A curried function (a -> b -> c):
-- This means f is a function that takes an argument of type a, returns another function (b -> c), which in turn takes a value of type b and produces a result of type c.

-- A tuple (a, b):
-- This is a single input containing two elements, the first of type a and the second of type b.
-- example ghci> :t (+)
-- (+) :: Num a => a -> a -> a

-- A function (a, b) -> c:
-- The function takes a tuple (a, b) as its input and directly produces a result of type c.


-- in simple word there is a function that take function , and take a and b and give output of c and then there is a and b for input and then they return c. 

uncurry :: (a->b->c) -> (a,b)->c -- again think. ß
uncurry f (x ,y) = f x y


multiply :: Int -> Int -> Int
multiply x y = x * y


-- we can use like this. 
plusN :: (Int , Int) -> Int
plusN (x , y) = uncurry (+) (x,y)


-- another way to define uncurry 


-- ghci> uncurry multiply (3,4)
-- 12



