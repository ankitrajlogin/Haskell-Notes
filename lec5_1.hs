

-- Explicit Effects 

------------------------------------------------
-- The original motivation for explicit effects
------------------------------------------------
-- • Given lazy evaluation as a strategy, the moment of evaluation is not easy to predict and hence not a good trigger for side-effecting actions.
-- • Even worse, it may be difficult to predict whether a term is evaluated at all.

-- • We would like to keep equational reasoning, and allow compiler optimisations such as
--        strictness analysis - evaluating things earlier than needed if they will definitely be needed, or
--         speculative evaluation - evaluating things even if they might not be needed at all.



------------------------------------------------ß
module IO where 

import Prelude hiding (getLine)

getLine :: String 
getLine = error "some magic operation" 

-- program1 : 
-- 
-- Read a Line and then read a second line and return both strings appended. 

program1 :: String
program1 = 
    let 
        x = getLine 
        y = getLine
    in 
        x ++ y 


program1' :: String
program1' = 
    getLine ++ getLine 


-- Program 2 ; 
-- 
-- Read a line and return the string appended to itself. 

program2 :: String 
program2 = 
    let 
        x = getLine 
    in
        x ++ x 





-- program 3 : 
-- 
-- Read a line and then read a secon dline and return both string appended with the second string appearing before the first. 

program3 :: String 
program3 =
    let 
        x = getLine 
        y = getLine 
    in 
        y ++ x 





-- OpenModuleSubstur code is trying to define getLine as a pure function returning a String, but getLine in Haskell is inherently an impure operation because it interacts with the outside world (i.e., reads user input). This causes problems in your programs because Haskell doesn't execute effects in a pure context.

-- Issues with Your Code
-- ---------------------------

-- getLine :: String is defined as a pure value, but it represents an impure operation.
-- In Haskell, the standard getLine function has type IO String, meaning it returns an IO action that produces a String.
-- Due to Haskell’s lazy evaluation, getLine is just a variable binding in your case, and calling it multiple times doesn’t actually perform different input operations.