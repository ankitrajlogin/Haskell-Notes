





--------------------------------------------
-- Binding Using <- (Inside do Notation)
--------------------------------------------
-- The <- operator is used inside a do block to extract values from IO actions.


main1 :: IO ()
main1 = do
    first <- getLine
    second <- getLine
    putStrLn ("Concatenated: " ++ first ++ " " ++ second)



-- Execution Steps
-- getLine waits for user input (e.g., "Ankit").
-- <- extracts "Ankit" from IO String.
-- putStrLn prints "Hello, Ankit".



-----------------------------------------------
-------------------------------------------------








--------------------------------------------
-- Using return with >>=
--------------------------------------------



-- main1 :: IO ()
-- main1 = getLine >>= \str -> (reverse str) >>= putStrLn

-- Error: reverse returns String, but >>= expects IO String.



-- Fix: return (reverse str) converts the result back to IO String.
main2 :: IO ()
main2 = getLine >>= \str -> return (reverse str) >>= putStrLn







--------------------------------------------
--------------------------------------------
-- >>= (Bind Operator)	Extracts the value from IO and passes it to the next function.
-- >> (Sequencing Operator)	Runs two IO actions ignoring the result of the first.
-- >> requires both sides to be IO actions.
--------------------------------------------
--------------------------------------------




main3 :: IO () 
main3 = getLine >>= \ _ -> return ( reverse "Hello Ankit") >> putStrLn "Hello World" 