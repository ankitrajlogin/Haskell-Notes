module Monads where 

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