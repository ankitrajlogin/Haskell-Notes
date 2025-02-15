
{-# OPTIONS_GHC -Wall #-}

module IO where 

import qualified Control.Applicative 


import Prelude hiding (sequence , liftA2)


liftA2 :: (a -> b-> c) -> IO a -> IO b -> IO c
liftA2 = Control.Applicative.liftA2 


ask :: String -> IO String 
ask prompt = 
    putStrLn prompt >> 
    getLine 


-- just converting above ask using do . 

ask' :: String -> IO String 
ask' prompt = do 
    putStrLn prompt 
    getLine 




askMany :: [String] -> IO [String]
askMany [] = return [] 
askMany (q : qs) = do 
    a <- ask q 
    as <- askMany qs 
    return (a : as)

 
-- ghci> askMany ["q1" , "q2"]
-- q1
-- ankit
-- q2
-- rahul
-- ["ankit","rahul"]



askMany' :: [String] -> IO [String]
askMany' [] = return []
askMany' (q : qs) = liftA2 (:) (ask q) (askMany' qs) 




sequence :: [IO a] -> IO [a]
sequence [] = return [] 
sequence (x : xs) = do 
    a <- x 
    as <- sequence xs
    return (a : as) 


sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (x : xs) = liftA2 (:) x (sequence' xs )



mainSeq :: IO() 
mainSeq = do 
    let actions = [getLine , getLine , getLine]
    result <- sequence actions 
    putStrLn "You Entered:" 
    print result


-- ghci> mainSeq
-- ankit
-- rahul
-- sayam
-- You Entered:
-- ["ankit","rahul","sayam"]




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



isEvenIO :: Int -> IO Bool
isEvenIO x = return (x `mod` 2 == 0)


main3 :: IO ()
main3 = do
    result <- filterM isEvenIO [1, 2, 3, 4, 5, 6]
    print result  -- Output: [2, 4, 6]


-- ghci> main3
-- [2,4,6]



askUser :: String -> IO Bool
askUser item = do
    putStrLn ("Do you want to keep " ++ item ++ "? (yes/no)")
    response <- getLine
    return (response == "yes")

main4 :: IO ()
main4 = do
    result <- filterM askUser ["apple", "banana", "cherry"]
    print result


-- ghci> main4
-- Do you want to keep apple? (yes/no)
-- yes
-- Do you want to keep banana? (yes/no)
-- no
-- Do you want to keep cherry? (yes/no)
-- yes
-- ["apple", "cherry"]



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




--------------------------------------------------
-- NOTE (diff between [ IO a ] and IO [a]
--------------------------------------------------

-- [IO a]	 -->   A list of IO actions that are not executed yet.
-- IO [a]	 -->    A single IO action that, when executed, runs all actions and returns a list of results.
