{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Prelude hiding ( and  , id) 


-- Understanding id in Haskell
-- In Haskell, id is the identity function. It simply returns its argument unchanged.

id :: a -> a
id x = x


-- This means:
-- id takes a value of any type a and returns it as is.

-- Why Use id?
-- Even though id looks trivial, it is very useful in functional programming, especially when working with higher-order functions, composition, and default behavior.


-----------------------------------------------------
-- 1. Higher-Order Functions (Default Behavior)
-----------------------------------------------------

-- When passing functions as arguments, sometimes we don’t want to transform anything. id can serve as a placeholder.

-- Example with map:

-- >>> map id [1,2,3,4]  -- [1,2,3,4]
-- [1,2,3,4]

-- id means "apply no transformation".
-- Equivalent to map (\x -> x) [1,2,3,4].
-- Example with filter:


-- >>> filter id [True, False, True, False]  -- [True, True]
-- [True,True]


-----------------------------------------------------
-- 2. Function Composition (.)
-----------------------------------------------------

-- In function composition, id acts as a neutral element (like 0 for addition or 1 for multiplication).

-- Example:

-- (f . id) x  ==  f x
-- (id . f) x  ==  f x
-- id does nothing in function composition.



-----------------------------------------------------
-- 3. Default Case in Conditional Logic
-----------------------------------------------------

-- When working with higher-order functions, id can be a fallback.

-- Example:


applyIfTrue :: Bool -> (a -> a) -> a -> a
applyIfTrue condition f  = (if condition then f else id) 

-- If condition is True, apply f.
-- Otherwise, apply id (which does nothing).
-- Usage:


-- >>> applyIfTrue True (*2) 10   
-- 20

-- >>> applyIfTrue False (*2) 10  -- 10 (id is applied)
-- 10





-----------------------------------------------------
-- 4. Simplifying Lambda Expressions
-----------------------------------------------------
-- Instead of:


-- \x -> x
-- We can just use:


-- id
-- This makes code shorter and clearer.






-----------------------------------------------------
-- 5. Useful in Monads and Functors
-----------------------------------------------------

-- In functors, fmap id should be equivalent to id:

-- >>> fmap id (Just 5)  ==  Just 5
-- True

-- In monads, return often acts like id:

-- return = id  -- In some contexts




-- Key Takeaways
-- ✅ Acts as a neutral function
-- ✅ Used in higher-order functions as a placeholder
-- ✅ Simplifies function composition
-- ✅ Eliminates unnecessary lambda expressions
-- ✅ Useful in monads and functors