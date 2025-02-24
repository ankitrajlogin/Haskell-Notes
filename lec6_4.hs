
module Monads where 

import Data.List (map) 

import Control.Applicative
import qualified  Data.Map.Strict as M 

import Prelude hiding (mapM, sequence)


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




returnMaybe :: a -> Maybe a 
returnMaybe x = Just x 

lookups1 :: Eq a => [a] -> [(a, b)] -> Maybe [b]
lookups1 keys tbl = 
    mapM (\key -> Prelude.lookup key tbl) keys


-- ghci> lookups1 [1, 2, 3, 4] (M.toList table)
-- Just [2,4,6,8]





lookups2 :: Ord a => [a] -> M.Map a b -> Maybe [b]
lookups2 keys tbl = 
    mapM (\key -> M.lookup key tbl) keys 






mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f  = sequence . map f 


-----------------------------------------------
-- converting [m a] -> m [a]
-----------------------------------------------


sequence :: Monad m => [m a] -> m [a]
sequence [] = return [] 
sequence (x : xs) = do 
    a <- x 
    as <- sequence xs 
    return (a : as) 



-----------------------------------------------
-- using same things using liftA2 
-----------------------------------------------




sequence2 :: Monad m => [m  a] -> m [a]
sequence2 [] = return []
sequence2 (x : xs) = liftA2 (:) x (sequence2 xs) 


