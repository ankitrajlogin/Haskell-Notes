


------------------------------------------------------------------
-- newtype Counter a = MkCounter { runCounter :: Int -> (a, Int) }
------------------------------------------------------------------


-- Breaking It Down
-- newtype Counter a → Defines a wrapper around a function.
-- MkCounter → The constructor that takes a function.
-- { runCounter :: Int -> (a, Int) } → Named field that:
-- Stores a function Int -> (a, Int).
-- Acts like a getter to retrieve the function inside MkCounter.





-- incrementCounter :: Int -> Counter Int
-- incrementCounter x = MkCounter (\count -> (x + 1, count + 1))


-- -- >>> runCounter (incrementCounter 34) 0
-- -- (35,1)










-------------------------------------------------------------------------
-- newtype Counter a = MkCounter { runCounter :: Int -> (a, Int) }
-- Splitting Counter into Two Parts
-------------------------------------------------------------------------

-- Instead of using newtype, we can define it using a data type and a function separately.

-- 1. Define CounterState Type for the Counter Function
type CounterState a = Int -> (a, Int)

-- This represents a function that takes an Int (counter value) and returns a new value (a) along with an updated counter.


-- 2. Define Counter as a Data Type
data Counter a = MkCounter (CounterState a)


-- 3. Rewriting runCounter
-- Since runCounter was originally just accessing the function inside MkCounter, we now explicitly extract and apply it:

runCounter :: Counter a -> Int -> (a , Int) 
runCounter (MkCounter f) val = f val 

-- Counter a → The function takes a value of type Counter a as its first argument.
-- Int → The function takes an Int as its second argument.
-- (a, Int) → The function returns a tuple, where:
-- a is the computed result (the main value produced by the counter).
-- Int is the updated counter value (i.e., an incremented or modified count).


incrementCounter :: Int -> Counter Int 
incrementCounter x = MkCounter (\count -> (x +1 , count +1)) 


-- >>> runCounter (incrementCounter 5) 0
-- (6,1)








----------------------------------------------------
--  Running the Counter
----------------------------------------------------
-- Let's now execute it:

-- runCounter (incrementCounter 5) 0


-- Step-by-step evaluation
-- 1. incrementCounter 5 creates:
-- MkCounter (\count -> (5 + 1, count + 1))

-- This function will take an Int and return (6, count + 1).


-- 2. Now apply runCounter:
-- runCounter (MkCounter (\count -> (6, count +1))) 0

-- Since runCounter (MkCounter f) val = f val, we get:
-- (\count -> (6, count +1)) 0

-- Substituting count = 0, we get:
-- (6, 1)
