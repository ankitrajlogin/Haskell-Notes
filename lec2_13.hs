{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (lookup)

import Control.Applicative 

example :: [(Int , String)]
example =
    [
        (1, "Ankit"),
        (2, "Rahul"),
        (3, "saham"),
        (11, "kaushik") , 
        (3, "dilip")
    ]

example2 :: [(Int , String)]
example2 = 
    [
        (21, "sunita")
    ]

data LookupResult val = LookupFailed | LookupSuccessful val 
    deriving Show 


lookup :: Eq key => key -> [(key ,val)] -> Maybe val 
lookup _ [] = Nothing
lookup key ((key' , val) : table) 
    | key == key' = Just val 
    | otherwise = lookup key table

-- ghci> :t Nothing
-- Nothing :: Maybe a

-- ghci> :t Just
-- Just :: a -> Maybe a


-- Maybe have two value, one -> Nothing , 2nd one -> Just x 


--------------------------------------
-- way to handle the nothing is default value 
--------------------------------------

-- for just x = x , for nothing def is used that is nothing. 

fromMaybe :: a -> Maybe a -> a
fromMaybe default_val Nothing = default_val
fromMaybe _default_val (Just x) = x

-- outputs : 

-- ghci> fromMaybe 'x' Nothing
-- 'x'

-- ghci> fromMaybe 4 Nothing
-- 4

-- ghci> fromMaybe 34 (Just 433)
-- 433

-- ghci> fromMaybe "None of the above" Nothing
-- "None of the above"

-- >>> fromMaybe "unknown" (lookup 3 example) 
-- "saham"

-- >>> fromMaybe "Unknown" (lookup 4 example)
-- "Unknown"




--------------------------------
---------------------------------------------

mapMaybe :: (a -> b ) -> Maybe a -> Maybe b -- available more generally as fmap or (<$>)
mapMaybe _f Nothing = Nothing
mapMaybe f (Just x) = Just (f x) 


-- output ; 
-- ghci> mapMaybe reverse (lookup 3 example)
-- Just "mahas"

-- >>> fmap (+1) (Just 3)
-- Just 4



---------------------------------------------
-- orelse  --> when first fail , then we use second one , and second one also fail, then nothing. 
---------------------------------------------

orelse :: Maybe a -> Maybe a -> Maybe a -- available more generally as (<|>)
orelse Nothing y = y
orelse (Just x) _ = Just x 

-- ghci> orelse (lookup 17 example) (lookup 21 example2)
-- Just "sunita"

-- orelse similar to || 
-- (||) :: Bool -> Bool -> Bool 
-- False || y = y 
-- True || _ = True 



-- >>> orelse (lookup 17 example) (lookup 22 example2)
-- Nothing



----------------------------------------
-- generalize version of orelse  (<|>)
-----------------------------------------

-- <|> :: Maybe a -> Maybe a -> Maybe a -- available more generally as (<|>)
-- <|> Nothing y = y
-- <|> (Just x) _ = Just x 



-- >>> lookup 17 example <|> lookup 21 example2
-- Just "sunita"
