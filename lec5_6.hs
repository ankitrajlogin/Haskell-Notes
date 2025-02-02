
{-# OPTIONS_GHC -Wall #-}

module IO where 

import qualified Control.Applicative 


import Prelude hiding (sequence)


liftA2 :: (a -> b-> c) -> IO a -> IO b -> IO c
liftA2 = Control.Applicative.liftA2 


ask :: String -> IO String 
ask prompt = 
    putStrLn prompt >> 
    getLine 


askMany :: [String] -> IO [String]
askMany [] = return [] 
askMany (q : qs) = do 
    a <- ask q 
    as <- askMany qs 
    return (a : as)

 
-- ghci> askMany ["q1" , "q2"]
-- q1
-- ankit raj
-- q2
-- rahul raj
-- ["ankit raj","rahul raj"]



askMany' :: [String] -> IO [String]
askMany' [] = return []
askMany' (q : qs) = liftA2 (:) (ask q) (askMany' qs) 


askMany'' :: [String] -> IO [String]
askMany'' [] = return []
askMany'' (q : qs) = liftA2 (:) (ask q) (askMany' qs) 


sequence :: [IO a] -> IO [a]
sequence [] = return [] 
sequence (x : xs) = do 
    a <- x 
    as <- sequence xs
    return (a : as) 


sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (x : xs) = liftA2 (:) x (sequence' xs )


filter' :: (a -> IO Bool) -> [a] -> IO [a]
filter' _p [] = return [] 
filter' p (x : xs)  = do
    b <- p x 
    if b 
        then do 
            ys <- filter' p xs 
            return (x : ys) 
        else filter' p xs  


filter'' :: (a -> IO Bool) -> [a] -> IO [a]
filter'' _p [] = return [] 
filter'' p (x : xs)  = do
    b <- p x 
    if b 
        then fmap (\ ys -> x : ys) (filter'' p xs) 
        else filter' p xs  

-- we can also write as. 
-- fmap as 
-- then (x : ) <$> filter'' p xs 


filter''' :: (a -> IO Bool) -> [a] -> IO [a]
filter''' _p [] = return [] 
filter''' p (x : xs)  = do
    b <- p x 
    if b 
        then (x:)  <$> filter''' p xs 
        else filter' p xs  



filterM :: (a -> IO Bool) -> [a] -> IO [a]
filterM _p [] = return [] 
filterM p (x : xs)  = do
    b <- p x 
    if b 
        then (x:)  <$> filterM p xs
        else filterM p xs  


askNULL :: String -> IO Bool
askNULL q = do 
    a <- ask q 
    return (null a) 

-- ghci> filter' askNULL ["q1" , "Q2" , "Q3"]
-- q1
-- ANKIT
-- Q2
-- R
-- Q3

-- ["Q3"]

