
{-# OPTIONS_GHC -Wall #-}

module IO where 

import Data.Char


-------------------------------------------
-- The pure Keyword in Haskell
-------------------------------------------
-- In Haskell, pure is part of the Applicative type class and serves a similar purpose to return in Monad. It is used to lift a pure value into a computational context (such as IO, Maybe, Either, etc.).



-------------------------------------------
-- Definition of pure
-------------------------------------------

-- pure :: Applicative f => a -> f a

-- It takes a value a and wraps it inside an Applicative functor f.
-- In the IO context, pure works like return, wrapping a pure value in an IO action.
-- In other contexts like Maybe, Either, or List, it creates minimal computational effects.




-------------------------------------------
-- Example 1: Using pure in IO
-------------------------------------------

main1 :: IO () 
main1 = do 
    x <- getLine 
    y <- pure (map toUpper x) 
    putStrLn x
    putStrLn y 

-- ghci> main1
-- ankit
-- ankit
-- ANKIT

-- Why use pure?

-- It makes the intent clear that we're dealing with Applicative and not Monad.
-- pure is often preferred in Applicative contexts, while return is commonly used in Monad contexts.



-- 🚨 If you don't use pure or return?
-------------------------------------------
-- You cannot bind (<-) a pure value in IO, so this would fail:

main2 :: IO ()
main2 = do
    x <- getLine
    let y = map toUpper x  -- ✅ Correct: `let` is for pure computations.
    putStrLn y





-------------------------------------------
-- Example 2: pure in Maybe
-------------------------------------------

example1 :: Maybe String
example1 = pure "Hello"  -- Just "Hello"

example2 :: Maybe Int
example2 = fmap (*2) (pure 10)  -- Just 20

-- pure "Hello" produces Just "Hello", lifting "Hello" into Maybe.
-- fmap (*2) (pure 10) applies (*2) to Just 10, resulting in Just 20.





-------------------------------------------
-- Key Differences: pure vs. return
-------------------------------------------

-- PURE 

-- Type -> Applicative	
-- Usage -> Lifts a value into any Applicative 
-- Common in -> Maybe, Either, [], IO, etc.	
-- Example	pure 5 :: [Int] → [5]




-- Return

-- Type ->  Monad
-- Usage -> Lifts a value into a Monad (like IO)
-- Common in -> Mostly IO computations
-- Example	-> return 5 :: IO Int → IO 5




-- Conclusion
-- ✅ Use pure when working with Applicatives, as it is more general.
-- ✅ Use return when working with Monads, especially in do notation with IO.