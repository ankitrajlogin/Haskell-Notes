
module Interaction where 

import System.IO 

-- (>>=) :: IO a -> (a -> IO b) -> IO b 

data Interaction = 
    Question String Interaction Interaction
    | Result String 

-- ghci> :t Question 
-- Question :: String -> Interaction -> Interaction -> Interaction
-- ghci> :t Result 
-- Result :: String -> Interaction


-- Key Takeaways
-- Constructors are functions that return the type they construct.
-- Question returns an Interaction, because:
-- It takes a string and two Interaction values.
-- The result is a valid Interaction (tree node).
-- Result returns an Interaction, because:
-- It takes a string.
-- It wraps the string into an Interaction (leaf node).
-- Recursive nature allows nested structures, forming decision trees.




example :: Interaction
example =
    Question "Do you like static types?"
        (Question "Do you like linear or affine types?"
            (Result "Try Rust!")
            (Question "Do you like dependent types?"
                (Result "Try Idris!")
                (Result "Try OCaml!")
            )
        )
        (Question "Do you like parentheses?"
            (Result "Try Clojure!")
            (Result "Try Erlang!")
        )



askYesNo :: String -> IO Bool
askYesNo q  = do 
    putStr (q ++ " [y/n] ")   -- Use putStr instead of putStrLn
    hFlush stdout  -- Ensure prompt appears immediately
    c <- getChar  
    putStrLn ""    -- Move to the next line after user input
    if c == 'y'
        then return True 
        else if c == 'n'
            then return False 
            else askYesNo q 


runInteraction :: Interaction -> IO ()
runInteraction (Question q y n) = do
    b <- askYesNo q
    if b  
        then runInteraction y  
        else runInteraction n  
runInteraction (Result r) = putStrLn r


-- ghci> runInteraction example
-- Do you like static types? [y/n] y
-- Do you like linear or affine types? [y/n] n
-- Do you like dependent types? [y/n] y
-- Try Idris!



simulate :: Interaction -> [Bool] -> Maybe String 
simulate (Question _ y n) (True : as) = simulate y as 
simulate (Question _ _ n) (False : as) = simulate n as 
simulate (Result r) [] = Just r 
simulate _ _ = Nothing 



-- ghci> simulate example [False , False , True]
-- Nothing

-- ghci> simulate example [True , True]
-- Just "Try Rust!"


-------------------------------------------------
-- place interaction 
-------------------------------------------------

-- example2 :: Interaction
-- example2 = 
--     Question "Where do you Live"
--         (Result "Nice")
--         (Question "Where do you live in India"
--             (Result "Nice")
--             (Question "Where do you live in Bihar"
--                 (Result "Nice")
--                 (Result "I also Live in Patna ")
--             )
--         )

-- fot this, for loop not work. as we need to input in same manner so that we can create recursion . 




-- Extended interaction with full branching (5-10 questions before an answer)
exampleExtended :: Interaction
exampleExtended =
    Question "Do you like programming?"
        (Question "Do you prefer low-level over high-level languages?"
            (Question "Do you enjoy manual memory management?"
                (Question "Do you prefer C over Rust?"
                    (Result "You should try C!")
                    (Result "You should try Rust!")
                )
                (Question "Do you like object-oriented programming?"
                    (Question "Do you prefer Java over Python?"
                        (Result "You should try Java!")
                        (Result "You should try Python!")
                    )
                    (Question "Do you prefer functional programming?"
                        (Result "You should try Haskell!")
                        (Result "You should try Scala!")
                    )
                )
            )
            (Question "Do you enjoy working with web technologies?"
                (Question "Do you like frontend development?"
                    (Question "Do you prefer React over Angular?"
                        (Result "You should try React!")
                        (Result "You should try Angular!")
                    )
                    (Question "Do you like backend development?"
                        (Result "You should try Node.js!")
                        (Result "You should try Django!")
                    )
                )
                (Question "Do you prefer statically typed languages?"
                    (Result "You should try Go!")
                    (Result "You should try TypeScript!")
                )
            )
        )
        (Question "Do you prefer working with data rather than code structure?"
            (Question "Do you like working with databases?"
                (Question "Do you prefer SQL over NoSQL?"
                    (Result "You should try PostgreSQL!")
                    (Result "You should try MongoDB!")
                )
                (Result "You should try Apache Cassandra!")
            )
            (Question "Do you like working with AI and Machine Learning?"
                (Question "Do you prefer working with deep learning?"
                    (Result "You should try TensorFlow!")
                    (Result "You should try Scikit-Learn!")
                )
                (Question "Do you prefer statistics-based models?"
                    (Result "You should try R!")
                    (Result "You should try MATLAB!")
                )
            )
        )



-- ghci> runInteraction exampleExtended
-- Do you like programming? [y/n] y
-- Do you prefer low-level or high-level languages? [y/n] h
-- Do you prefer low-level or high-level languages? [y/n] n
-- Do you enjoy working with web technologies? [y/n] y
-- Do you like frontend development? [y/n] y
-- Do you prefer React over Angular? [y/n] y
-- You should try React!