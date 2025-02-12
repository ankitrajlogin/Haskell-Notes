
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
module FunctorFoldable where


data NewDef a = LeftBool Bool | RightVal a (NewDef a)
    deriving (Show, Functor)


-----------------------------------------
-- How Functor Works for NewDef
-----------------------------------------

-- The derived Functor instance will behave as:

-- instance Functor NewDef where
--     fmap _ (LeftBool b) = LeftBool b  -- Ignore the Bool value
--     fmap f (RightVal x rest) = RightVal (f x) (fmap f rest)


tree :: NewDef Integer
tree = RightVal 3 (RightVal 5 (LeftBool True))

-- >>> fmap (*2) tree
-- RightVal 6 (RightVal 10 (LeftBool True))


-- Step-by-step transformation:
-----------------------------------------

-- fmap (*2) (RightVal 3 (RightVal 5 (LeftBool True)))
-- = RightVal (3 * 2) (fmap (*2) (RightVal 5 (LeftBool True)))
-- = RightVal 6 (RightVal (5 * 2) (fmap (*2) (LeftBool True)))
-- = RightVal 6 (RightVal 10 (LeftBool True))



-----------------------------------------
-- When Functor Cannot Be Derived
-- -----------------------------------------
-- A data type cannot be an instance of Functor if:

-- The type variable a appears in a contravariant position (e.g., in function arguments).
-- The type is not fully applied (kind mismatches).
-- Example: Function in Contravariant Position

data Contra a = Contra (a -> Int)

-- fmap is supposed to apply a function (a -> b), but a appears as an input in a -> Int, making it impossible to map over a.







-----------------------------------------
-- When Foldable Cannot Be Derived
-- -----------------------------------------
-- A data type cannot be an instance of Foldable if:

-- The type variable a does not appear in the structure at all.
-- The structure does not contain multiple a values to fold over.

data Phantom a = Phantom Int


--  Cannot derive instance Foldable Phantom
--   because ‘a’ does not appear in the data type