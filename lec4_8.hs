{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
module FunctorFoldable where


import Prelude 




mapList :: (a->b) -> [a] -> [b]
mapList _ [] = []
mapList f (x : xs) = f x : mapList f xs

mapMaybe :: (a->b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing 
mapMaybe f (Just x) = Just (f x) 



data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show





mapTree :: (a->b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r) 




-- ghci> elem 3 (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- False

-- ghci> null (Leaf 'x')
-- False

-- ghci> product  (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- 140



------------------------------------------------
-- What is a Functor in Haskell?
------------------------------------------------

-- A Functor in Haskell is a type class that allows you to apply a function to a wrapped value inside a context (such as Maybe, List, or Either). It provides a uniform way to transform the contents of a structure without changing its shape.



-- myself define functor in name of MyFunctor 

class MyFunctor f where
    myfmap :: (a->b) -> f a -> f b
    

-- f is a type constructor (like Maybe, [] (list), Either e, etc.).
-- fmap takes a function (a -> b) and applies it to f a, producing f b.
-- The structure (f) remains the same; only the values inside change.



instance MyFunctor [] where 
    -- myfmap :: (a->b) -> [] a -> [] b 
    myfmap :: (a->b) -> [a] -> [b]
    myfmap = mapList 

instance MyFunctor Maybe where
    myfmap :: (a->b) -> Maybe a -> Maybe b
    myfmap = mapMaybe

instance MyFunctor Tree where 
    myfmap :: (a->b) -> Tree a -> Tree b
    myfmap = mapTree 

-- data Either a b = Left a | Right b 

instance MyFunctor (Either x) where 
    myfmap :: (a->b) -> Either x a -> Either x b
    myfmap _f (Left x) = Left x 
    myfmap f (Right a) = Right (f a) 

-- if it is left , then left and else for right , f function apply to the value a . 


-- ghci> :t myfmap
-- myfmap :: MyFunctor f => (a -> b) -> f a -> f b



-- ghci> :i myfmap
-- type MyFunctor :: (* -> *) -> Constraint
-- class MyFunctor f where
--   myfmap :: (a -> b) -> f a -> f b
--         -- Defined at lec4_8.hs:61:5



-- ghci> myfmap (+1) Nothing
-- Nothing

-- ghci> myfmap (+1) [234,234,234]
-- [235,235,235]

-- ghci> myfmap (+1) (Just 433)
-- Just 434

-- ghci> myfmap (+4) (Node (Leaf 3) (Leaf 8))
-- Node (Leaf 7) (Leaf 12)


-- ghci> myfmap (+1) (Left 3)
-- Left 3

-- ghci> myfmap (+1) (Right 3)
-- Right 4




---------------------------------------
-- default myfmap is fmap in functor class. 
---------------------------------------


-- ghci> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b


-- ghci> :i Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--         -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Functor Solo -- Defined in ‘GHC.Base’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Functor (Either a) -- Defined in ‘Data.Either’




--   (<$) :: a -> f b -> f a

-- ghci> 3 <$ [1..10]
-- [3,3,3,3,3,3,3,3,3,3]


-- ghci> 'x' <$ Node (Leaf 3) (Leaf 8)
-- Node (Leaf 'x') (Leaf 'x')



-- ghci> Left 'x' :: Either Char Bool
-- Left 'x'
-- ghci> Right True :: Either Char Bool
-- Right True



-- ghci> fmap (+1) (Right 3)
-- Right 4
-- ghci> fmap (+1) (Left 3)
-- Left 3

-- ghci> fmap (+1) (3,4)
-- (3,5)


-- ghci> myfmap (+1) [43,3,453,43,43]
-- [44,4,454,44,44]


-- ghci> fmap (+1) ('x' , 'f')
-- <interactive>:58:7: error:
--     • No instance for (Num Char) arising from a use of ‘+’
--     • In the expression: (+)
--       In the first argument of ‘fmap’, namely ‘(+ 1)’
--       In the expression: fmap (+ 1) ('x', 'f')

-- ghci> fmap (+1) ('x' , 34)
-- ('x',35)










----------------------------------
-- product :: (Foldable t, Num a) => t a -> a
--------------------------------

-- Explanation:
-- t is a type constructor that represents a container or a data structure that holds elements of type a.

-- The constraint (Foldable t) means that t must be a type that implements the Foldable type class, which includes data structures like lists ([]), Maybe, Tree, etc.

-- The constraint (Num a) means that a must be a numeric type, like Int, Integer, Double, etc.

-- t a means that t is a container that holds elements of type a.

-- The function product takes such a container (t a) and returns a single numeric value of type a, which is the product of all elements.





-------------
-- tolist 
-----------

-- ghci> toList (Just 5)
-- [5]

-- ghci> toList Nothing
-- []










----------------------------------------ß
-------------- Foldable ---------------
----------------------------------------

-- ghci> :i Foldable
-- type Foldable :: (* -> *) -> Constraint
-- class Foldable t where
--   Data.Foldable.fold :: Monoid m => t m -> m
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   Data.Foldable.foldMap' :: Monoid m => (a -> m) -> t a -> m
--   foldr :: (a -> b -> b) -> b -> t a -> b
--   Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
--   foldl :: (b -> a -> b) -> b -> t a -> b
--   Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
--   foldr1 :: (a -> a -> a) -> t a -> a
--   foldl1 :: (a -> a -> a) -> t a -> a
--   Data.Foldable.toList :: t a -> [a]
--   null :: t a -> Bool
--   length :: t a -> Int
--   elem :: Eq a => a -> t a -> Bool
--   maximum :: Ord a => t a -> a
--   minimum :: Ord a => t a -> a
--   sum :: Num a => t a -> a
--   product :: Num a => t a -> a
--   {-# MINIMAL foldMap | foldr #-}
--         -- Defined in ‘Data.Foldable’
-- instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
-- instance Foldable (Either a) -- Defined in ‘Data.Foldable’
-- instance Foldable Maybe -- Defined in ‘Data.Foldable’
-- instance Foldable Solo -- Defined in ‘Data.Foldable’
-- instance Foldable [] -- Defined in ‘Data.Foldable’



-- ghci> length (Left 'x')
-- 0
-- ghci> length (Right 'x')
-- 1


-- Defining new instance of Tree
toList :: Tree a -> [a]
toList (Leaf x) = [x] 
toList (Node l  r) = toList l ++ toList r




-- instance Foldable Tree where 
--     length :: Tree a -> Int 
--     length (Leaf _) = 1  
--     length (Node left right) = 1 + length left + length right 

-- Here, you are defining length manually, but Foldable does not require length to be explicitly implemented.
-- Foldable expects foldr, and functions like elem and length are automatically derived from foldr.
-- Since foldr is not implemented, Haskell does not know how to process elem.






instance Foldable Tree where 
    foldr :: (a-> b -> b) -> b -> Tree a -> b 
    foldr cons nil t = foldr cons nil (toList t)


-- > elem is based on foldr , hence when elem we call, it call foldr and hence , all it first convert it into list. 


-- >>> elem 7 (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- True

-- internally it work like this because elem function use foldr internally. 


------------------------------------------
-- How elem Work 
-------------------------------------------

-- The elem function is defined as:

-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem y = any (== y)

-- which internally calls foldr:
-- any p = foldr (\x acc -> p x || acc) False'




-- >>> elem 7 (toList (Node (Leaf 5) (Node (Leaf 7) (Leaf 4))))
-- True


-- >>> maximum (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- 7


-- >>> sum (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- 16


-- >>> length (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- 3



-- but 
-- >>> length (3,4)
-- 1

-- because (3,4) pair consider as one. 

-- >>> length (Left 'x') 
-- 0

-- >>> length (Right 'x')
-- 1


-- behaves this way is due to how Foldable is implemented for Either in Haskell.

---------------------------------------
-- Understanding Foldable for Either
---------------------------------------
-- Haskell provides a built-in Foldable instance for Either e:


-- instance Foldable (Either e) where
    -- foldr _ acc (Left _)  = acc
    -- foldr f acc (Right x) = f x acc
-- This means:

-- Left e is ignored completely in foldr.
-- Only Right x is processed.



-- Hence 
-- Since Left 'x' is ignored by foldr, the result is just the initial accumulator 0.


-- similarly 


---------------------------------------------
-- for pair 
---------------------------------------------

-- >>> elem 3 (3,4) 
-- False

-- >>> elem 3 (4,3) 
-- True


-- instance Foldable ((,) a) where
--     foldr f acc (_, x) = f x acc

-- This means:

-- Only the second element of the tuple is considered for folding.
-- The first element is completely ignored in foldr.








------------------------------------------------
-- above we have use langguage derive for foldable and functor  
------------------------------------------------


data NewTree a = LeafN a | NodeN (NewTree a) (NewTree a) 
    deriving (Show , Functor , Foldable) 


-- now , we can use functor and foldable function in NewTree also 

-------------------------------------------
-- 1. Understanding Foldable for NewTree
-------------------------------------------
-- Since we derived Foldable automatically:

-- data NewTree a = LeafN a | NodeN (NewTree a) (NewTree a)
--     deriving (Show, Functor, Foldable)


-- Haskell automatically implements Foldable NewTree in a way that traverses all values (a) in the tree.


-------------------------------------------
-- How Functor Works for NewTree
-- -------------------------------------------
-- You derived Functor for NewTree:


-- instance Functor NewTree where
--     fmap f (LeafN x) = LeafN (f x)
--     fmap f (NodeN left right) = NodeN (fmap f left) (fmap f right)

-- What does fmap do?
-- It applies a function f to every a in the NewTree a.
-- It does not modify the structure of the tree.
-- It only affects values inside LeafN (not NodeN itself).



-- >>> fmap (+1) (NodeN (LeafN 3) (LeafN 5)) 
-- NodeN (LeafN 4) (LeafN 6)

-- >>> sum (NodeN (LeafN 3) (LeafN 5)) 
-- 8


-- >>> elem 3 (NodeN (LeafN 3) (LeafN 5)) 
-- True

-- >>> length (Node (Leaf 5) (Node (Leaf 7) (Leaf 4))) 
-- 3




