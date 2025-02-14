

import qualified Control.Applicative

-------------------------------------------------------
-- Understanding >>= (Bind Operator) in Haskell
-------------------------------------------------------
-- The bind operator (>>=) is used in Haskell to sequence computations in a monadic context, particularly in IO operations. It takes a monadic value (IO a) and a function (a -> IO b), then extracts the value from the first computation and passes it to the next function.


-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- Where:

-- m a is a monadic value (e.g., IO a).
-- (a -> m b) is a function that takes an a and returns a new monadic value m b.
-- The result is m b, keeping the computation in the same monadic context.



-------------------------------------------------------
-- 🚀 Different Use Cases of >>=
-------------------------------------------------------
-- 1️⃣ Basic Example: Getting User Input and Using It

main1 :: IO ()
main1 = getLine >>= \name -> putStrLn ("Hello, " ++ name)

-- ✅ Explanation:
-- getLine is an IO String (monadic action).
-- >>= takes the user input (name) and passes it to putStrLn.
-- The final output is "Hello, <user-input>".




-------------------------------------------------------
-- 2️⃣ Multiple >>= Usage (Chaining Multiple IO Actions)
-------------------------------------------------------


main2 :: IO ()
main2 = getLine >>= \firstName ->
       getLine >>= \lastName ->
       putStrLn ("Hello, " ++ firstName ++ " " ++ lastName)


-- Explanation:

-- First, getLine reads firstName.
-- Then, getLine reads lastName.
-- Finally, putStrLn prints "Hello, firstName lastName".




-------------------------------------------------------
-- 3️⃣ Using >>= with return
-------------------------------------------------------


main3 :: IO ()
main3 = getLine >>= \name -> return ("Welcome " ++ name) >>= putStrLn


-- ✅ Explanation:

-- getLine gets the user's input (name).
-- return ("Welcome " ++ name) wraps the string inside IO.
-- >>= passes this value to putStrLn.
-- Example Run:





-------------------------------------------------------
-- 4️⃣ Ignoring a Value (>> vs. >>=)
-------------------------------------------------------
-- Using >>= (value is used)




-- ✅ Explanation:

-- getLine reads input but ignores it (\_ ->).
-- "You entered something!" is printed.


-- Using >> (value is discarded)

main4' :: IO ()
main4' = getLine >> putStrLn "You entered something!"

-- ✅ Explanation:

-- >> just sequences the two actions, discarding getLine’s result.
-- 🔹 Key Difference:

-- >>= extracts the value and passes it forward.
-- >> ignores the value and just moves to the next computation.




-------------------------------------------------------
-- 5️⃣ >>= with Custom Functions
-------------------------------------------------------

processInput :: String -> IO ()
processInput input = putStrLn ("Processed: " ++ reverse input)

main5 :: IO ()
main5 = getLine >>= processInput

-- ✅ Explanation:

-- getLine gets user input (IO String).
-- >>= passes this string to processInput, which prints the reversed string.



-------------------------------------------------------
-- 6️⃣ >>= in List Monad (Non-IO Example)
-- -------------------------------------------------------
listExample :: [Int]
listExample = [1, 2, 3] >>= \x -> [x, x * 2]


-- ✅ Explanation:
-- >>= applies the function (\x -> [x, x * 2]) to each element in [1, 2, 3].
-- The function doubles each number and returns a flattened list.

-- Output:
-- [1,2,2,4,3,6]





-------------------------------------------------------
-- 🎯 Key Takeaways
-- -------------------------------------------------------
-- Case	What Happens?
-- Basic IO Chaining   	getLine >>= \name -> putStrLn name extracts input and prints it.
-- Multiple IO Actions	         getLine >>= \x -> getLine >>= \y -> putStrLn (x ++ y) chains inputs.
-- Using return	           return wraps a value in IO, but >>= is needed to extract it.
-- Using >> vs. >>=	             >> ignores the result of the first action; >>= passes it.
-- With Custom Functions	          >>= allows functions to process extracted values.
-- In List Monad	          >>= applies a function and flattens the list.



-- -------------------------------------------------------
-- 💡 Summary
-- -------------------------------------------------------
-- >>= extracts the value from IO a and passes it forward to a function.
-- >> ignores the extracted value and just runs the next action.
-- return wraps a pure value in IO, but doesn’t "extract" it.
