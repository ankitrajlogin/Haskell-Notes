module Monads where 
import Control.Monad.State

---------------------------------------------------------------
-- Understanding Monads and the Bind (>>=) Operator in Haskell
---------------------------------------------------------------

-- A Monad is a design pattern in Haskell that allows chaining computations while handling effects like state, I/O, failure, or non-determinism.

-- Monads have three key properties:

-- They wrap a value in a computational context (e.g., IO a, Maybe a, Either e a).
-- They allow chaining computations using >>= (bind operator).
-- They ensure computations follow specific rules (Monad Laws: Identity, Associativity).



-- class Monad m where
--     return :: a -> m a        -- Wrap a value into the monad
--     (>>=)  :: m a -> (a -> m b) -> m b   -- Bind operator
--     (>>)   :: m a -> m b -> m b  -- Sequence actions, discarding first result



-- return takes a normal value and puts it inside a monadic context.
-- >>= (bind) applies a function to a monadic value.
-- >> sequences two monadic actions, discarding the first result.


-- Common Types of Monads and Their Uses
---------------------------------------------------------------


---------------------------------------------------------------
-- 1. Maybe Monad (Handles Optional Values)
---------------------------------------------------------------

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing   -- Avoid division by zero
safeDivide a b = Just (a `div` b)

example :: Maybe Int
example = Just 10 >>= \x -> safeDivide x 2 >>= \y -> Just (y + 1)



-- Breakdown of >>= Usage

-- Just 10 >>= \x -> safeDivide x 2 → Passes 10 to safeDivide.
-- If safeDivide x 2 is Nothing, computation stops.
-- Otherwise, y + 1 is wrapped in Just.





-------------------------------------------------------------
-- 2. IO Monad (Handles Input/Output)
---------------------------------------------------------------

main :: IO ()
main = do
    name <- getLine  -- Get user input
    putStrLn ("Hello, " ++ name)  -- Print message


-- Using >>= Instead of do

main2 :: IO ()
main2 = getLine >>= \name -> putStrLn ("Hello, " ++ name)




---------------------------------------------------------------
-- 3. Either Monad (Handles Errors)
---------------------------------------------------------------

safeRoot :: Double -> Either String Double
safeRoot x
    | x < 0     = Left "Negative input is not allowed"
    | otherwise = Right (sqrt x)

example2 :: Either String Double
example2 = Right 16 >>= safeRoot >>= \y -> Right (y * 2)


-- Use Case: Error handling with informative messages.






---------------------------------------------------------------
-- 4. List Monad (Handles Non-Deterministic Computations)
---------------------------------------------------------------


pairs :: [(Int, Int)]
pairs = [1,2] >>= \x -> [3,4] >>= \y -> return (x, y)


-- Same as list comprehension:
pairs' = [(x, y) | x <- [1,2], y <- [3,4]]





---------------------------------------------------------------
-- 5. State Monad (Handles State Changes)
---------------------------------------------------------------



increment :: State Int Int
increment = do
    n <- get
    put (n + 1)
    return n


example3 :: (Int, Int)
example3 = runState increment 10  -- Output: (10, 11)

-- >>> example3
-- (10,11)



filteredEvens :: [Int]
filteredEvens = [1,2,3,4] >>= \x -> if even x then [x] else []

-- >>> filteredEvens
-- [2,4]







---------------------------------------------------------------
-- Code: Generating Pythagorean Triples Using the List Monad
---------------------------------------------------------------


pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples =
    [1..20] >>= \x ->        -- 1st number (x) ranges from 1 to 20
    [x..20] >>= \y ->        -- 2nd number (y) starts from x to 20
    [y..20] >>= \z ->        -- 3rd number (z) starts from y to 20
    if x*x + y*y == z*z      -- Check if (x, y, z) satisfies the Pythagorean theorem
        then return (x, y, z)  -- If true, include (x, y, z) in the result
        else []               -- If false, discard it



-- Step-by-Step Breakdown
---------------------------------------------------------------

-- Let's expand how this works for small numbers (1 to 5) before generalizing it.

-- First Monad Bind: [1..5] >>= \x -> ...

-- This means x will take values from [1,2,3,4,5] one by one.
-- First, x = 1, then x = 2, and so on.
-- Second Monad Bind: [x..5] >>= \y -> ...

-- For each x, y starts from x to 5.
-- If x = 1, then y will take values [1,2,3,4,5].
-- If x = 2, then y will take values [2,3,4,5], and so on.
-- Third Monad Bind: [y..5] >>= \z -> ...

-- For each (x, y), z starts from y to 5.
-- If (x, y) = (1,2), then z will take values [2,3,4,5].
-- Filtering with Pythagorean Theorem:


-- if x*x + y*y == z*z then return (x, y, z) else []


-- output 
-- [(3,4,5), (6,8,10), (5,12,13), (9,12,15), (8,15,17), (12,16,20)]


pythagoreanTriples' :: [(Int, Int, Int)]
pythagoreanTriples' = [(x, y, z) | x <- [1..20], y <- [x..20], z <- [y..20], x*x + y*y == z*z]



