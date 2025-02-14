
{-# OPTIONS_GHC -Wall #-}

module IO where 

import Data.Char

import Control.Applicative

import Prelude hiding ((>>))


-------------------------------
-- Bind Operator (>>=)
-------------------------------
--  (>>=) :: IO a -> (a -> IO b) -> IO b

-- First argument: IO a (an IO action that produces a value of type a)
-- Second argument: A function that takes a and returns IO b
-- Result: IO b (another IO action)





-- import qualified Control.Applicative



-- Bind :: Letting an action use an earlier result 

-- ghci> :t getLine 
-- getLine :: IO String

-- ghci> :t putStrLn
-- putStrLn :: String -> IO ()

(>>) :: IO a -> IO b -> IO b 
(>>) = liftA2 (\ _a b -> b) 


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

-- Step-by-Step Execution:
-- getLine waits for user input and stores it in x.
-- >>= binds the user input (x) to the function that follows.
-- putStrLn x prints the input once.
-- >> sequences the next putStrLn x, printing it again


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

-- ghci> :t toUpper 
-- toUpper :: Char -> Char
-- toUpper is char to char. so need to use map to map each char. 

-- USE OF IN -- 
-- in putStrLn y >> putStrLn y
-- The in keyword makes y available only in the putStrLn y >> putStrLn y expression.
-- >> sequences two putStrLn actions to print y twice.


-- Alternative: Using do Notation
shoutBackTwice'' :: IO () 
shoutBackTwice'' = do
    x <- getLine       -- Read input and bind to x
    let y = map toUpper x  -- Define y inside the do-block
    putStrLn y        -- Print y
    putStrLn y        -- Print y again


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



-------------------------------------------------------
-------------------------------------------------------


fmap' :: (a-> b) -> IO a -> IO b 
fmap' f ioa = 
    ioa >>=  \a -> return (f a) 


-- This type signature tells us:
-- First parameter: (a -> b), a function that takes a value of type a and returns a value of type b.
-- Second parameter: IO a, an IO action that produces a value of type a when executed.
-- Return type: IO b, an IO action that produces a value of type b when executed.



-- ioa is an IO Action
-- ioa has the type IO a, meaning it is an action that, when executed, will produce a value of type a.


-- The bind operator (>>=) is used to extract the value from the IO action.
-- ioa >>= \a -> means:
-- Execute ioa (which is an IO a).
-- Bind the result to a (i.e., take the actual value inside the IO).
-- Pass a to the lambda function (\a -> ...).



-- Example If ioa = getLine, it will:

-- Execute getLine to read a string from the user.
-- Bind the user's input to a.



-- ghci> fmap' reverse getLine
-- ankit
-- "tikna"

main4 :: IO ()
main4 = fmap' reverse getLine >>= putStrLn

-- ghci> main4
-- ankit
-- tikna




-------------------------------------------------------
-------------------------------------------------------


liftA2' :: (a-> b-> c) -> IO a -> IO b -> IO c 
liftA2' f ioa iob = 
    ioa >>= \a ->
    iob >>= \ b -> 
    return (f a b)





main5 :: IO () 
main5 = liftA2' (++) getLine getLine  >>= putStrLn 


-- ghci> main5
-- ankit
-- raj
-- ankitraj



