
-- In Haskell, an IO String represents an effectful computation that, when executed, will produce a String. You cannot directly convert IO String to String, but you can extract the String from the IO context inside the do block or using the bind (>>=) operator.

-- Ways to Extract String from IO String
-------------------------------------------------

import Control.Monad (liftM)


-------------------------------------------------
-- 1. Using do Notation 
-------------------------------------------------

main1 :: IO ()
main1 = do
    str <- getLine  -- `str` is now a `String`, extracted from `IO String`
    putStrLn ("You entered: " ++ str)


-- Explanation:
-- <- extracts the String from IO String.
-- Now str is a pure String and can be used normally.




-------------------------------------------------
-- 2. Using the Bind (>>=) Operator (For One-Liners)
-------------------------------------------------

main2 :: IO ()
main2 = getLine >>= \str -> putStrLn ("You entered: " ++ str)


-- Explanation:
-- >>= takes IO String, extracts the String, and passes it to the lambda function (\str ->).




-------------------------------------------------
-- 3. Using liftM from Control.Monad (Alternative to >>=)
-------------------------------------------------


-- import Control.Monad (liftM)

main3 :: IO ()
main3 = liftM reverse getLine >>= putStrLn
















-------------------------------------------------
-- Example 
-------------------------------------------------

processString :: String -> String
processString = reverse  -- Example: Just reverses the string

main5 :: IO () 
main5 = getLine >>= \ str -> putStrLn ( processString str )



main6 :: IO ()
main6 = do
    str <- getLine
    putStrLn (processString str)



-- Explanation of the Fix

-- <- extracts the String from IO String
-- processString str is a pure String
-- putStrLn (processString str) prints the result


