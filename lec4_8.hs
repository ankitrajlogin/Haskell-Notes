{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
module FunctorFoldable where




mapList :: (a->b) -> [a] -> [b]
mapList _ [] = []
mapList f (x : xs) = f x : mapList f xs

mapMaybe :: (a->b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing 
mapMaybe f (Just x) = Just (f x) 



data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show



toList :: Tree a -> [a]
toList (Leaf x) = [x] 
toList (Node l  r) = toList l ++ toList r

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r) 


instance Foldable Tree where 
    foldr :: (a->b -> b) -> b -> Tree a -> b
    foldr cons nil t = foldr cons nil (toList t) 


-- ghci> elem 3 (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- False

-- ghci> null (Leaf 'x')
-- False

-- ghci> product  (Node (Leaf 5) (Node (Leaf 7) (Leaf 4)))
-- 140


class MyFunctor f where
    myfmap :: (a->b) -> f a -> f b

instance MyFunctor [] where 
    myfmap :: (a->b) -> [a] -> [b]
    myfmap = mapList 

instance MyFunctor Maybe where
    myfmap :: (a->b) -> Maybe a -> Maybe b
    myfmap = mapMaybe

instance Functor Tree where 
    fmap :: (a->b) -> Tree a -> Tree b
    fmap = mapTree 

-- ghci> fmap (+1) Nothing
-- Nothing

-- ghci> fmap (+1) [234,234,234]
-- [235,235,235]

-- ghci> myfmap (+1) (Just 433)
-- Just 434


-- ghci> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b


-- ghci> 3 <$ [1..10]
-- [3,3,3,3,3,3,3,3,3,3]


-- ghci> 'x' <$ Node (Leaf 3) (Leaf 8)
-- Node (Leaf 'x') (Leaf 'x')






-- Data Either a b = Left a | Right b

-- ghci> Left 'x' :: Either Char Bool
-- Left 'x'
-- ghci> Right True :: Either Char Bool
-- Right True

instance MyFunctor (Either x) where 
    myfmap :: (a->b) -> Either x a -> Either x b
    myfmap _f (Left x) = Left x 
    myfmap f (Right a) = Right (f a) 

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





-------------- Foldable ---------------

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



