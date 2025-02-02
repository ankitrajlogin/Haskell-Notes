
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExtendedDefaultRules #-}


{-# LANGUAGE NoMonomorphismRestriction #-}


-- The monomorphism restriction is a rule in Haskell that applies to bindings (values or functions) that lack explicit type signatures and do not take arguments. Under this rule, these bindings are assigned a monomorphic (specific) type instead of a polymorphic (general) type, in certain cases.

-- This restriction is a subtle aspect of Haskell’s type inference system and is meant to balance performance and clarity in type resolution.


-- module MonomorphismRestriction where 

minimal :: Bounded a => a
minimal = minBound 


f:: [Char]
f = reverse "hello"



equalToItself :: Eq a => a -> Bool
equalToItself = \y -> y == y



equalToItself' :: Eq a => a -> Bool
equalToItself' y = y == y



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


