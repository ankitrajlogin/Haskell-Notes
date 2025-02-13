
{-# OPTIONS_GHC -Wall #-}

module MonomorphismRestriction where 

    
-- {-# LANGUAGE ExtendedDefaultRules #-}


-- {-# LANGUAGE NoMonomorphismRestriction #-}

----------------------------------------------------
-- What is monomorphismRestriction in Haskell?
----------------------------------------------------
-- The monomorphism restriction (MR) in Haskell is a type system rule that limits certain expressions from having a polymorphic type unless explicitly given a type annotation.

-- By default, Haskell allows polymorphism, meaning functions and expressions can have generic types (using type variables). However, in some cases, due to performance reasons and ambiguity resolution, Haskell applies the monomorphism restriction, forcing an expression to have a more specific (monomorphic) type.



minimal :: Bounded a => a
minimal = minBound 



equalToItself :: Eq a => a -> Bool
equalToItself = \y -> y == y



equalToItself' :: Eq a => a -> Bool
equalToItself' y = y == y



f:: [Char]
f = reverse "hello"



x :: Integer 
x = 42 


-- ghci> :t 42
-- 42 :: Num a => a
-- ghci> :t x 
-- x :: Integer


default (Int) 

sf :: Int 
sf = 23



-- ghci> [] :: [Int]
-- []
-- ghci> [] :: [()]
-- []
-- ghci> [] :: [Char]
-- ""


strange :: String -> String
-- strange ss = show (read ss ) 
strange xx = show (read xx +1 :: Int)


-- ghci> strange "34"
-- "35"

-- ghci> strange "hello"
-- "*** Exception: Prelude.read: no parse




-- import Text.Read 
-- strangeSafe :: String -> Maybe String
-- strangeSafe xx = case readMaybe xx :: Maybe Int of 
--     Just n -> Just (show (n +1)) 
--     Nothing -> Nothing 





y :: Num a => a -> a
y = \f -> f + f






-------------------------------------------------
-- Where Can You Use Monomorphism Restriction?
-------------------------------------------------

-- The monomorphism restriction (MR) is useful when you need performance optimization or disambiguation of type inference. Here are some practical scenarios where MR plays a role:



-- Performance Optimization: Avoiding Redundant Computation
-------------------------------------------------
-- Haskell lazily evaluates expressions, meaning a polymorphic function or expression might be recomputed multiple times if it lacks MR.



-------------------------------------------------
-- Example Without Monomorphism Restriction
-------------------------------------------------

-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- expensiveComputation = sum [1..1000000]

-- expensiveComputation does not have a monomorphic type because we disabled MR.
-- If used multiple times, it may be recomputed for different types each time.





-------------------------------------------------
-- Example With Monomorphism Restriction
-------------------------------------------------

-- expensiveComputation :: Int  -- Explicitly specifying type
-- expensiveComputation = sum [1..1000000]

-- MR forces expensiveComputation to have a fixed type (Int).
-- The result is cached and not recomputed.

