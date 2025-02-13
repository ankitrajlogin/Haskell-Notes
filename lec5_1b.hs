

-- Your code is trying to define getLine as a pure function returning a String, but getLine in Haskell is inherently an impure operation because it interacts with the outside world (i.e., reads user input). This causes problems in your programs because Haskell doesn't execute effects in a pure context.

-- Issues with Your Code
-- getLine :: String is defined as a pure value, but it represents an impure operation.
-- In Haskell, the standard getLine function has type IO String, meaning it returns an IO action that produces a String.
-- Due to Haskell’s lazy evaluation, getLine is just a variable binding in your case, and calling it multiple times doesn’t actually perform different input operations.



--------------------------------------
-- How to Correct the Code
--------------------------------------


-- To properly execute getLine, you need to use the IO monad.


module IOExample where 

import Prelude hiding (getLine)
import System.IO (getLine)



-- Program 1: Read two lines and append them.
program1 :: IO String
program1 = do
    x <- getLine
    y <- getLine
    return (x ++ y)



-- Program 2: Read a line and append it to itself.
program2 :: IO String
program2 = do
    x <- getLine
    return (x ++ x)




-- Program 3: Read two lines and append them in reverse order.
program3 :: IO String
program3 = do
    x <- getLine
    y <- getLine
    return (y ++ x)



-- ghci> program1
-- ankit
-- raj
-- "ankitraj"


-- ghci> program2
-- ankit
-- "ankitankit"


-- ghci> program3
-- ankit
-- raj
-- "rajankit"

----------------------------------------------------
-- Key Fixes
-- ----------------------------------------------------

-- Use IO String as the return type instead of String because reading input is an effectful operation.
-- Use do notation to sequence multiple IO actions.
-- Use <- to extract values from IO String, since getLine is an action that produces a String, not just a String.



-- ----------------------------------------------------
-- How It Works
-- ----------------------------------------------------

-- getLine is an IO action, so it must be executed within IO to get a string.
-- <- is used to extract values from IO.
-- return is used to wrap the final result back into IO.


main :: IO ()
main = do
    result <- program1  -- Or program2 / program3
    putStrLn result


-- ghci> main
-- ankit
-- raj
-- ankitraj







-- ----------------------------------------------------
-- Using >> (Ignore Result)
-- ----------------------------------------------------

main3 :: IO ()
main3 = getLine >> putStrLn "Hello!" 


main4 :: IO () 
main4 = 
    do
        x <- getLine 
        putStrLn ("Hello!" ++ x )






-----------------------------------------------------
-- x <- getLine Cannot Be Used Outside do Notation
------------------------------------------------------
-- main5 :: IO() 
-- main5 = x <-getLine >> putStrLn ("hello" ++ x) 



-- Mistake: You are trying to bind (<-) getLine to x outside of a do block.

-- In Haskell, x <- getLine only works inside do notation, because <- extracts the value from an IO action.
-- Outside do notation, you must use the >>= (bind) operator.



main5 :: IO ()
main5 = do
    x <- getLine
    putStrLn ("hello" ++ x)




------------------------------------------------------
-- Using >>= (Without do Notation)
------------------------------------------------------


main6 :: IO() 
main6 = getLine >>= \x -> putStrLn ("Hello" ++ x) 


-- why this works?

-- >>= (bind operator) extracts the String result from getLine.
-- \x -> putStrLn ("hello" ++ x) is an anonymous function (lambda) that takes x and prints the final output.




-- main7 :: IO () 
-- main7 = \x -> putStrLn ("hello" ++ x) >>= getLine 


--  Mistake:

-- putStrLn ("hello" ++ x) >>= getLine is incorrect because:
-- >>= binds an IO action's result to a function.
-- But putStrLn returns IO (), which does not produce a meaningful value for getLine to use.






------------------------------------------------------
-- Error: >>= Cannot Be Used with Non-IO Values 
------------------------------------------------------


-- work :: Int -> Int 
-- work a = a*2 >>= \x -> x +2 >>= \x -> x/2


-- >>= (bind operator) is for IO or other monadic values, not plain integers.
-- Here, a * 2, x + 2, and x / 2 are all plain Int or Fractional values, not monadic (IO Int, Maybe Int, etc.).
-- The bind operator (>>=) only works with monads, so using it on Int causes a type error.





------------------------------------------------------
-- Use the Maybe Monad (If Needed)
------------------------------------------------------

-- If you meant to handle optional values (e.g., avoiding division by zero), you could use the Maybe monad:


work :: Int -> Maybe Int 
work a = Just (a * 2) >>= \x -> Just (x + 2) >>= \x -> if x /= 0 then Just (x `div` 2) else Nothing


-- Why this works?
----------------------------------------

-- Just (a * 2) makes it monadic (Maybe Int).
-- >>= correctly chains the computations.
-- Handles division by zero safely.




-- Key Takeaways
-- >>= is for monads (IO, Maybe, etc.), not plain values like Int.
-- Use normal function composition (+, *, /) for arithmetic operations.
-- If handling optional values, wrap them in a monad (Maybe, IO, etc.).




work3 :: IO ()
work3 = do
    x <- getLine
    let name = "Hello, " ++ x
    putStrLn ("Welcome " ++ name)


