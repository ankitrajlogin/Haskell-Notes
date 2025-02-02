
{-# OPTIONS_GHC -Wall #-}

module IO where 

import Data.Char


-- import qualified Control.Applicative



-- Bind :: Letting an action use an earlier result 

-- ghci> :t getLine 
-- getLine :: IO String

-- ghci> :t putStrLn
-- putStrLn :: String -> IO ()


echo :: IO () 
echo = getLine >>= putStrLn

-- (>>=) :: IO a -> (a-> IO b) -> IO b

-- (>>=) getLine :: (String -> IO b) -> IO b

-- (>>=) getLine putStrLn :: IO () 


echoReverse :: IO ()
echoReverse = getLine >>= \ x -> putStrLn (reverse x)

-- ghci> echoReverse
-- ankitraj
-- jartikna


echoTwice :: IO ()
echoTwice = getLine >>= \ x -> putStrLn x >> putStrLn x

-- ghci> echoTwice
-- ankit
-- ankit
-- ankit



shoutBackTwice :: IO ()
shoutBackTwice = getLine >>= \x -> let y = map toUpper x in putStrLn y >> putStrLn y

-- ghci> shoutBackTwice
-- ankit
-- ANKIT
-- ANKIT


shoutBackTwice' :: IO() 
shoutBackTwice' = 
    getLine >>= \x -> 
    let y = map toUpper x 
    in putStrLn y >> putStrLn y 


combineLines :: IO () 
combineLines = 
    getLine >>= \x -> 
    getLine >>= \y ->
    putStrLn ( x ++ " , " ++ y) 


-- ghci> combineLines
-- ankit
-- raj
-- ankit , raj


-- (>>) :: IO a -> IO b -> IO b
-- ioa >> iob = ioa >>= \_ -> iab

-- return :: a -> IO a 

fmap' :: (a-> b) -> IO a -> IO b 
fmap' f ioa = 
    ioa >>=  \a -> return (f a) 


liftA2' :: (a-> b-> c) -> IO a -> IO b -> IO c 
liftA2' f ioa iob = 
    ioa >>= \a ->
    iob >>= \ b -> 
    return (f a b)




