
module Monads where 

import Data.Map.Strict as M

-- ghci> :i Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   {-# MINIMAL (>>=) #-}
--         -- Defined in ‘GHC.Base’
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monad ((,,) a b)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c)
--   -- Defined in ‘GHC.Base’
-- instance Monad ((->) r) -- Defined in ‘GHC.Base’
-- instance Monad IO -- Defined in ‘GHC.Base’
-- instance Monad Maybe -- Defined in ‘GHC.Base’
-- instance Monad Solo -- Defined in ‘GHC.Base’
-- instance Monad [] -- Defined in ‘GHC.Base’
-- instance Monad (Either e) -- Defined in ‘Data.Either’




table  :: M.Map Int Int
table = 
    M.fromList 
    [ (1,2) , 
    (2 , 4) , 
    ( 3, 6) , 
    (4, 8 ) , 
    ( 5, 10 ) , 
    ( 6 , 12) , 
    (8 , 16) , 
    (10 , 20 ) , 
    (15 , 30) , 
    (16 , 32) , 
    ( 20 , 40 ) , 
    (32 , 64) 
    ]


(>>>=) :: Maybe a -> (a-> Maybe b) -> Maybe b 
comp >>>= rest = 
    case comp of
        Nothing -> Nothing 
        Just x -> rest x 

threeLookups2 :: Int -> Maybe Int
threeLookups2 i0 = 
    M.lookup i0 table >>>= \i1 -> 
    M.lookup i1 table >>>= \i2 -> 
    M.lookup i2 table >>>= \i3 ->
    return (i3 +1)




-- ghci> threeLookups2 5
-- Just 41



returnMaybe :: a -> Maybe a 
returnMaybe x = Just x 

threeLookups2' :: Int -> Maybe Int
threeLookups2' i0 = 
    M.lookup i0 table >>>= \i1 -> 
    M.lookup i1 table >>>= \i2 -> 
    M.lookup i2 table >>>= \i3 ->
    returnMaybe (i3 +1)



-------------------------------------------
-- writing using do block 
-------------------------------------------

threeLookups3 :: Int -> Maybe Int
threeLookups3 i0 = do
    i1 <- M.lookup i0 table 
    i2 <- M.lookup i1 table 
    i3 <- M.lookup i2 table
    return (i3 +1)


-- ghci> threeLookups3 6
-- Nothing



--------------------------------------------------
-- Sequence 
--------------------------------------------------

-- seqeunce :: [IO a] -> IO [a]
-- sequence :: [Maybe a] -> Maybe [a]



-- ghci> :t sequence
-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)

-- ghci> sequence [Just 3 , Just 4 , Just 7 , Just 10]
-- Just [3,4,7,10]






--------------------------------------------------
-- mapM s
--------------------------------------------------


-- ghci> :t mapM (\ x -> M.lookup x table) [1,2,3,4]
-- mapM (\ x -> M.lookup x table) [1,2,3,4] :: Maybe [Int]


-- ghci>  mapM (\ x -> M.lookup x table) [1,2,3,4]
-- Just [2,4,6,8]


-- ghci> mapM (\ x -> M.lookup x table) [1,2,3,4,5, 6 , 7, 8]
-- Nothing
--- (if any one fails , hole lookup will fail) 

