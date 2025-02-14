

import Control.Applicative (pure)
import Language.Haskell.TH (safe)

-- A function that takes a string, asks for input, processes it, and prints a result.


processNumber :: String -> IO () 
processNumber name = do 
    putStrLn ("Hello, " ++ name ++ "! Enter a number :")
    input <- getLine 
    let number = read input :: Int 
    let doubled = (*2) <$> Just number 
    putStrLn ("Doubled number is : " ++ show doubled) 






-- Another function using `>>=` and `pure`

getUserInput :: IO Int 
getUserInput = do 
    putStrLn "Enter another number:" 
    getLine >>= \input -> pure (read input :: Int)


-- 1. What Does getLine Do?
--------------------------------------
-- getLine :: IO String
-- getLine is an IO action that reads a string from the user.
-- Since it returns IO String, we need to transform this into an IO Int.


-- 2. What Happens with >>=?
--------------------------------------
-- getLine >>= \input -> pure (read input :: Int)
-- >>= (bind operator) takes getLine (which is IO String).
-- It extracts the inner value (String), and passes it to the lambda function \input -> ....


-- 3. Role of pure
--------------------------------------
-- pure (read input :: Int)
-- read input :: Int is a pure computation that converts a string into an integer.
-- However, getUserInput must return IO Int, not just Int.
-- pure lifts the pure value (read input :: Int) into IO, making it IO Int.


-- summary 
--------------------------------------
-- pure wraps a normal value (Int) into IO Int.
-- It ensures that getUserInput has the correct return type (IO Int).
-- It avoids unnecessary use of do notation in this case.
-- pure is equivalent to return in IO, but pure emphasizes applicative style.



replaceWithMessage :: IO () 
replaceWithMessage = do 
    userInput <- getUserInput 
    putStrLn $ "Your squared number is : " ++ show (userInput * userInput) 

-- $ ensures that everything after it is evaluated first, then passed to putStrLn.



main :: IO () 
main = do 
    processNumber "Ankit" 
    replaceWithMessage 



-- ghci> main 
-- Hello, Ankit! Enter a number :
-- 5
-- Doubled number is : Just 10
-- Enter another number:
-- 4
-- Your squared number is : 16

