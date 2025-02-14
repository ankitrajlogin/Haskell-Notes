module Main where 



main :: IO () 
main = putStrLn "Hello World" 






myFunction :: String -> IO Int
myFunction name = do
    putStrLn ("Hello, " ++ name ++ "!")
    putStrLn "Enter a number:"
    input <- getLine
    let number = read input :: Int
    return (number * 2)


--------------------------------------
-- NOTE 
--------------------------------------
-- IO and Purity
-- Haskell is a purely functional language, meaning functions must not have side effects unless explicitly marked as such. Any function that performs I/O operations (like putStrLn or getLine) must return an IO type.




-- putStrLn and getLine perform side effects (printing to and reading from the console).
-- Since the function involves I/O, the entire computation is inside the IO monad.
-- return (number * 2) does not exit the IO context; it returns IO Int, not just Int.

