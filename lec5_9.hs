
module Interaction where 


-- (>>=) :: IO a -> (a -> IO b) -> IO b 

data Interaction = 
    Question String Interaction Interaction
    | Result String 


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
    putStrLn (q ++ " [y/n]")  
    c <- getChar  
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



simulate :: Interaction -> [Bool] -> Maybe String 
simulate (Question _ y n) (True : as) = simulate y as 
simulate (Question _ _ n) (False : as) = simulate n as 
simulate (Result r) [] = Just r 
simulate _ _ = Nothing 

