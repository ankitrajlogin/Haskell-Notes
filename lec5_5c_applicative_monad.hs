

-------------------------------------------
-- Understanding Applicative and Monad in Haskell
-------------------------------------------
-- Both Applicative and Monad are abstractions in Haskell that help work with computations wrapped inside a context (like IO, Maybe, Either, etc.).

-------------------------------------------
-- 1. Functor → Applicative → Monad Hierarchy
-------------------------------------------

-- Haskell has three key abstractions for working with effects:

-- 1. Functor (fmap): Allows applying a function to a wrapped value.
-- 2. Applicative (<*> & pure): Allows applying wrapped functions to wrapped values.
-- 3. Monad (>>= & return): Allows sequencing computations where the next step depends on the previous result.



--    Functor     (fmap)
--       ↓
--  Applicative  (pure, <*>)
--       ↓
--    Monad      (return, >>=)




-------------------------------------------
-- 1. Functor
-------------------------------------------
-- A Functor is any type that can be mapped over using fmap.

-- Example: Functor with Maybe

-- fmap (+1) (Just 5)  -- Just 6
-- fmap (+1) Nothing   -- Nothing

-- fmap applies (+1) to Just 5, producing Just 6.

-- Nothing remains Nothing.






-------------------------------------------
-- 2. Applicative
-------------------------------------------
-- An Applicative Functor extends Functor by allowing functions inside a context to be applied to values inside a context.

-- Definition of Applicative

-- class Functor f => Applicative f where
--     pure  :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b



-- pure lifts a value into an Applicative.
-- <*> applies a function wrapped inside a context to another wrapped value.

-- Example: Applicative with Maybe

-- Just (+1) <*> Just 5   -- Just 6
-- Just (+) <*> Just 3 <*> Just 4   -- Just 7
-- Nothing <*> Just 5     -- Nothing

-- Just (+1) is a function wrapped inside Maybe, applied to Just 5.
-- Just (+) <*> Just 3 <*> Just 4 adds both numbers inside Maybe.


-- 🚨 Why Applicative?
-- Applicative allows function application while handling missing values (Nothing).






-------------------------------------------
-- 3. Monad
-------------------------------------------
-- A Monad extends Applicative by allowing computations where the result of one step determines the next step.

-- Definition of Monad

-- class Applicative m => Monad m where
--     (>>=)  :: m a -> (a -> m b) -> m b
--     return :: a -> m a  -- Same as `pure`


-- >>= (bind) takes a wrapped value (m a) and a function (a -> m b), chaining computations.
-- return (same as pure) lifts a value into the monad.


-- Example: Monad with Maybe
-- Just 3 >>= \x -> Just (x + 1)  -- Just 4
-- Nothing >>= \x -> Just (x + 1) -- Nothing
-- >>= ensures that if a computation fails (Nothing), the whole chain fails.


-- 🚨 Why Monad?
-- It allows chaining dependent computations while handling effects.




-------------------------------------------
-- Comparison of Functor, Applicative, and Monad
-------------------------------------------

-- Concept	(Operator)	-> Description

-- Functor	(fmap) -> 	Apply a function to a wrapped value
-- Applicative (<*>, pure) -> 	Apply a wrapped function to a wrapped value
-- Monad  (	>>=, return ) -> 	Chain computations where each step depends on the previous result



-- When to Use What?
-- Use Functor (fmap) when you only need to apply a function inside a context.
-- Use Applicative (pure, <*>) when you have functions and values inside a context.
-- Use Monad (>>=) when computations depend on previous results.