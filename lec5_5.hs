

-- Purpose of do Notation
-- The do notation allows you to write a sequence of monadic operations in a clear and imperative style, without manually chaining them with monadic bind (>>=) or sequencing operator (>>). It makes code involving monads easier to read and understand

{-# OPTIONS_GHC -Wall #-}

module IO where 

import Data.Char


-- import qualified Control.Applicative




conversation :: IO () 
conversation = 
    putStrLn "Please provide some input." >>  
    getLine >>= \x -> 
    if null x
        then putStrLn "Bye!"
        else putStrLn ("Thanks for the input (" ++ x ++ "). ") >> conversation 



-- ghci> conversation
-- Please provide some input.
-- ankit raj
-- Thanks for the input (ankit raj). 
-- Please provide some input.

-- Bye!


conversation1 :: IO ()
conversation1 =
  ask "Please provide some input." 
  >>= \x ->
    if null x
      then putStrLn "Bye!"
      else putStrLn ("Thanks for the input (" ++ x ++ ").") >> conversation1


ask :: String -> IO String
ask prompt =
  putStrLn prompt >>
  getLine


-- ghci> ask "Ankit"
-- Ankit
-- rahul
-- "rahul"

--------------------------------------------------------
-- Why is "rahul" printed even though you're not explicitly printing it?
--------------------------------------------------------

-- This happens because in GHCi, when you call ask "Ankit", the result of the IO String action is implicitly printed by GHCi.

-- Let's break it down:
-- 1. putStrLn prompt prints "Ankit" to the console.
-- 2. >> sequences the action, discarding the result of putStrLn prompt.
-- 3. getLine reads input from the user (e.g., "rahul").
-- 4. Since ask is an IO String, it returns the user input ("rahul").
-- 5. In GHCi, when an IO action produces a result, GHCi automatically prints it. That's why "rahul" appears on the screen.


--------------------------------------------------------
-- How to prevent GHCi from printing the return value?
--------------------------------------------------------

-- If you don’t want GHCi to print the returned value, you can explicitly discard it using _ <- ask "Ankit" or run ask "Ankit" >> return () in GHCi.



-- ghci> _ <- ask "Ankit"
-- Ankit
-- rahul


liftA2' :: (a-> b -> c) -> IO a -> IO b -> IO c 
liftA2' f ioa iob = 
  ioa >>= \ a -> 
  iob >>= \b -> 
  return (f a b) 


-- ghci> liftA2' (++) getLine getLine 
-- ankit
-- rahul
-- "ankitrahul"





-----------------------------------------------------------
-----------------------------------------------------------
--  USE OF DO NOTATION FOR THIS SAME TASK
-----------------------------------------------------------
-----------------------------------------------------------




conversation2 :: IO ()
conversation2 = do
  x <- ask2 "Please provide some input."
  if null x
    then putStrLn "Bye!"
    else do
      putStrLn ("Thanks for the input (" ++ x ++ ").")
      conversation2  -- Recursive call to continue the conversation


ask2 :: String -> IO String
ask2 prompt = do
  putStrLn prompt
  getLine


-- ghci> conversation2
-- Please provide some input.
-- ankit raj
-- Thanks for the input (ankit raj).
-- Please provide some input.

-- Bye!

testing :: IO String 
testing = getLine >>= \x -> return ("Hello , " ++ x ++ " ") ; 




liftA2'' :: (a->b->c) -> IO a -> IO b -> IO c 
liftA2'' f ioa iob = do 
    a <- ioa 
    b <- iob 
    return (f a b) 


-- ghci> liftA2' (++) getLine getLine 
-- ankit
-- rahul
-- "ankitrahul"



-- ghci> liftA2' (++) testing testing
-- ankit
-- rahul
-- "Hello , ankit Hello , rahul "



shoutBackTwice :: IO() 
shoutBackTwice = 
    getLine >>= \x -> 
    let y = map toUpper x 
    in putStrLn y >> putStrLn y 


shoutBackTwice' :: IO () 
shoutBackTwice' = do 
    x <- getLine 
    let y = map toUpper x 
    putStrLn y 
    putStrLn y 

shoutBackTwice'' :: IO () 
shoutBackTwice'' = do 
    x <- getLine 
    y <- return (map toUpper x) 
    putStrLn y 
    putStrLn y 



question :: String -> IO() 
question q = 
    putStrLn (q ++ "?")

-- ghci> question "ankit"
-- ankit?






-- difference between manually and do notation. 

normal :: IO ()
normal =
  putStrLn "What is your name?" >>
  getLine >>= \name ->
  putStrLn ("Hello, " ++ name ++ "!")
  


withdo :: IO ()
withdo = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")



-- Creating a Loop in do

loop :: IO ()
loop = do
  putStrLn "Enter something (or press Enter to quit):"
  input <- getLine
  if null input
    then putStrLn "Goodbye!"
    else do
      putStrLn ("You entered: " ++ input)
      loop  -- Recursive call to continue the loop


