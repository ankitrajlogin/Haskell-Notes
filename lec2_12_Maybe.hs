
{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (zip , zipWith , lookup)


-- Handling lookup when system get not key. 

example :: [(Int , String)]
example =
    [
        (1, "Frodo"),
        (2, "Bilbo"),
        (3, "Dandalf"),
        (11, "Lego") , 
        (3, "salman")
    ]

data LookupResult val = LookupFailed | LookupSuccessful val 
    deriving Show 


lookup :: Eq key => key -> [(key , val)] -> (key , LookupResult val) 
lookup key [] = (key , LookupFailed )
lookup key ((key' , val) : table) = if key == key' then (key , LookupSuccessful val) else lookup key table 

-- ghci> lookup 4 example 
-- (4,LookupFailed)


-- ghci> lookup 3 example 
-- (3,LookupSuccessful "Dandalf")



-- ghci> :i Maybe
-- type Maybe :: * -> *
-- data Maybe a = Nothing | Just a
--         -- Defined in ‘GHC.Maybe’
-- instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (Maybe a)
--   -- Defined in ‘GHC.Base’
-- instance Foldable Maybe -- Defined in ‘Data.Foldable’
-- instance Traversable Maybe -- Defined in ‘Data.Traversable’
-- instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
-- instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance Ord a => Ord (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
-- instance Applicative Maybe -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance MonadFail Maybe -- Defined in ‘Control.Monad.Fail’
-- instance Monad Maybe -- Defined in ‘GHC.Base’


-- we can use maybe also to control crash 

-- data Maybe a = Nothing | Just a

lookup' :: Eq key => key -> [(key ,val)] -> Maybe val 
lookup' _ [] = Nothing
lookup' key ((key' , val) : table) 
    | key == key' = Just val 
    | otherwise = lookup' key table


lookup2 :: Eq key => key -> [(key , val)] -> Maybe val 
lookup2 _ [] = Nothing 
lookup2 key ((key' , val) : table) =
    if key == key' then Just val else lookup2 key table 


-- ghci> lookup2 3 example 
-- Just "Dandalf"

-- ghci> lookup2 4 example
-- Nothing


lookup3 :: Eq key => key -> [(key , val)] -> (key , Maybe val) 
lookup3 key [] = (key , Nothing )
lookup3 key ((key' , val) : table) = if key == key' then (key , Just val) else lookup3 key table 

-- ghci> lookup3 4 example
-- (4,Nothing)

