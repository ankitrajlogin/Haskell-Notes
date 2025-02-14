

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



liftA2' :: (a->b->c) -> IO a -> IO b -> IO c 
liftA2' f ioa iob = do 
    a <- ioa 
    b <- iob 
    return (f a b) 



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


