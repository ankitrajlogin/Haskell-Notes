{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding (reverse ,  elem  , and , foldr  ,flip , (.) , id , ($) , fst , map ) 


-- parse :: String -> a
-- parse "False" = False
-- parse "0" = 0 
-- parse _ = "unknown"

-- The implementation parse "False" = False returns a Bool (False), which is a specific type.
-- However, the type signature says the function should work for any type a, and it should return a value of type a. This is inconsistent because False is always of type Bool and cannot be generalized to every type a.


-- How to Fix it ( Specify the Output Type) 
parse :: String -> Bool
parse "False" = False
parse _       = True 



-- Option 2 : Use Either 

-- ghci> :i Either
-- type Either :: * -> * -> *
-- data Either a b = Left a | Right b
--         -- Defined in ‘Data.Either’

parse2 :: String -> Either (Either Bool Int) String
parse2 "False" = Left (Left False)
parse2 "0" = Left (Right 0) 
parse2 _ = Right "Unknown"


-- Rather than direct use. we can first define a data type and then difine it. 

data ParseResult = Abool Bool | AnInt Int | Astring String 
    deriving Show

parse3 :: String -> ParseResult 
parse3 "False" = Abool False 
parse3 "0" = AnInt 0 
parse3 _ = Astring "Unknown"






-- -- | A JSON \"object)" (key)/value map).
-- type Object = HashMap Text Value

-- -- | A JSON \"array\" (sequence).
-- type Array = Vector Value

-- -- | A JSON value represented as a Haskell value.
-- data Value = Object !0bject
--     | Array !Array
--     | String !Text
--     | Number !Scientific
--     | Bool !Bool
--     | Null
--         deriving (Eq, Read, Typeable, Data, Generic)


-- -- | Since version 1.5.6.0 version object values are printed in lexicographic key order
-- --
-- -- >>> toJSON $ H.fromList [("z", False), ("a", True)]
-- -- Object (fromList [("a",Bool True), ("z",Bool False)])

-- instance Show Value where
--     showsPrec _ Null = showString "Null"
--     showsPrec d (Bool b) = showParen (d > 10)
--         $ showString "Bool " . showsPrec 11 b
--     showsPrec d (Number s) = showParen (d > 10)
--         $ showString "Number " . showsPrec 11 s
--     showsPrec d (String s) = showParen (d > 10)
--         $ showString "String " . showsPrec 11 s
--     showsPrec d (Array xs) = showParen (d > 10)
--         $ showString "Array " . showsPrec 11 xs
--     showsPrec d (Object xs) = showParen (d > 10)
--         $ showString "Object (fromList "
--         . showsPrec 11 (sortBy (comparing fst) (H.toList xs))
--         . showChar ')'



