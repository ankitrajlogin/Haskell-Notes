






-- newtype Counter a = MkCounter (Int -> (a, Int))
-- also write this as below 

newtype Counter a = MkCounter { runCounter :: Int -> (a, Int) }

-- runCounter unwraps the function inside MkCounter.
-- It allows us to apply the wrapped function to an integer.



-----------------------------------------------------------------------
-- NOTE : MKcounter is expects a function of type Int -> (a , Int) , not a plain value . 
-----------------------------------------------------------------------



------------------------------------------------------------------------
-- Defining a function that return Counter a , mins that MkCounter and a function that finally return the pair (a , Int) 
------------------------------------------------------------------------


-- Function to increment a number and update the counter
incrementCounter :: Int -> Counter Int
incrementCounter x = MkCounter (\count -> (x + 1, count + 1))


-- runCounter is a field accessor.
-- It extracts the function wrapped inside MkCounter.


-----------------------------------------------------------------
-- runCounter only expects a Counter a.
-- ✔ It returns a function of type Int -> (a, Int).
-- ✔ That returned function expects an Int and produces (a, Int).
-----------------------------------------------------------------------

-- >>> runCounter (incrementCounter 34) 0
-- (35,1)


-- >>> runCounter (MkCounter (\count -> (1 , count+1))) 324
-- (1,325)



-- Function to square a number and update the counter
squareCounter :: Int -> Counter Int
squareCounter x = MkCounter (\count -> (x * x, count + 1))

-- >>> runCounter (squareCounter 23) 0
-- (529,1)






-- Function to chain operations inside Counter
bindCounter :: Counter a -> (a -> Counter b) -> Counter b
bindCounter (MkCounter f) g = MkCounter $ \count ->
    let (result, newCount) = f count
        MkCounter h = g result
    in h newCount

-- 🛠 Key Components
-- First argument (Counter a)

-- This is a stateful computation that takes an Int counter and returns (a, Int).
-- Second argument (a -> Counter b)

-- This is a function that, given a result of type a, produces another stateful computation Counter b.
-- How It Works

-- It runs the first computation to get a and an updated counter.
-- It applies g to a, which gives a new Counter b.
-- It runs this new computation with the updated counter.









-- Function to run a full computation
runExample :: Int -> (Int, Int)
runExample x = runCounter finalCounter 0
  where
    finalCounter = incrementCounter x `bindCounter` squareCounter



-- Main function to test it
main :: IO ()
main = do
    let (result, finalCount) = runExample 4
    putStrLn $ "Final Result: " ++ show result
    putStrLn $ "Final Count: " ++ show finalCount
