
module Monads where 

import Data.List (map) 

import Control.Applicative
import Data.Map.Strict as M 

import Prelude hiding (mapM, sequence)


returnMaybe :: a -> Maybe a 
returnMaybe x = Just x 

lookups1 :: Eq a => [a] -> [(a, b)] -> Maybe [b]
lookups1 keys tbl = 
    mapM (\key -> Prelude.lookup key tbl) keys



lookups2 :: Ord a => [a] -> M.Map a b -> Maybe [b]
lookups2 keys tbl = 
    mapM (\key -> M.lookup key tbl) keys 



mapM :: Monad m => (a => m b) -> [a] -> m [b]
mapM f xs = sequence . map f xs 

sequence :: Monad m => [m a] -> m [a]
sequence [] = return [] 
sequence (x : xs) = do 
    a <- x 
    as <- sequence xs 
    return (a : as) 


sequence2 :: Monad m => [m  a] -> m [a]
sequence2 [] = return []
sequence2 (x : xs) = liftA2 (:) x (sequence2 xs) 


