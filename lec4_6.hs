{-# LANGUAGE InstanceSigs #-}


module Overloading where

import Text.Read 

------------ SHOW CLASS ------------------


-- ghci> :i Show
-- type Show :: * -> Constraint
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS
--   {-# MINIMAL showsPrec | show #-}
--         -- Defined in ‘GHC.Show’


-- ghci> show False
-- "False"

-- ghci> show True
-- "True"

-- ghci> show 34234
-- "34234"

-- ghci> show ankitraj
-- <interactive>:32:6: error: Variable not in scope: ankitraj

-- ghci> show (False , [1,2,3])
-- "(False,[1,2,3])"

-- ghci> show (Just(Just "323"))
-- "Just (Just \"323\")"

-- ghci> show (Just (False, [1,2,3]))
-- "Just (False,[1,2,3])"


-- ghci> :t print
-- print :: Show a => a -> IO ()



data Choice = Rock | Paper | Scissors


instance Show Choice where 
    show :: Choice -> String
    show Rock = "r"
    show Paper = "p"
    show Scissors = "s"

-- ghci> Rock
-- r

-- ghci> [Rock, Paper , Scissors]
-- [r,p,s]

-- ghci> show ['a' , 'b']
-- "\"ab\""



-- ghci> :t read
-- read :: Read a => String -> a

-- ghci> read "False"
-- *** Exception: Prelude.read: no parse
-- ghci> read "False" :: Bool
-- False

-- ghci> read "123" :: Int
-- 123
-- ghci> read "1243" + 34
-- 1277

-- ghci> read "[1,2,3]" ++ "[4,5,6]"
-- "*** Exception: Prelude.read: no parse

-- ghci> read "[1,2,3]" ++ [4,5,6]
-- [1,2,3,4,5,6]


----------- redMaybe ------------------------
--------------------------------------------

-- readMaybe is a safe way to parse a String into a value of a specified type. It is part of the Text.Read module and is preferred over the standard read function when you want to avoid runtime errors caused by parsing failures.


-- ghci> readMaybe "False" :: Maybe Bool
-- Just False

-- ghci> readMaybe "False" :: Maybe Int
-- Nothing


-- ghci> readMaybe "()"
-- Just ()

-- ghci> readMaybe "False"
-- Nothing


-- ghci> strange x = show (read x)

-- ghci> :t strange
-- strange :: String -> String






-- try to show that is store. 

-- It show error. message is : 
-- strange x = show (read x) 

-- Ambiguous type variable ‘a0’ arising from a use of ‘show’
--   prevents the constraint ‘(Show a0)’ from being solved.
--   Probable fix: use a type annotation to specify what ‘a0’ should be.
--   Potentially matching instances:
--     instance (Show a, Show b) => Show (Either a b)
--       -- Defined in ‘Data.Either’
--     instance Show Lexeme -- Defined in ‘Text.Read.Lex’




strange :: String -> String
strange x = show (read x :: Int)

-- ghci> strange "234"
-- "234"

-- ghci> strange "x"
-- "*** Exception: Prelude.read: no parse

