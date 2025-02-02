

module Into where


-- If you want to use the name not, you can hide the not function from Prelude when importing it:
import Prelude hiding (not , (||) , elem)


(||) :: Bool -> Bool -> Bool 
False || y = y 
True || y = True 


elem :: Eq a => a -> [a] -> Bool 
elem x [] = False 
elem x (y : ys) = (x == y) || (elem x ys) 


-- equational Reasoning 

-- elem 9 [6,9,42]
-- ->  elem 9 (6 : (9 : (42: [])))
-- ->  9 == 6 || elem 9 （9：42：［J）
-- ->  False || elem 9 (9 : 42 : [])
-- ->  elem 9 (9 : 42 : [])
-- ->  9 == 9 || elem 9 (42 : [])
-- ->  True || elem 9 (42 : [])
-- ->  True




-------------------------------------------
-- Lazy Evaluation in GHCI 
-------------------------------------------

-- Haskell uses lazy evaluation, meaning expressions are not evaluated until their values are actually needed. This allows Haskell to work with infinite lists, avoid unnecessary computations, and improve efficiency.


-- Laziness with Infinite Lists

-- One of the biggest advantages of laziness is the ability to work with infinite lists.

-- Example: Infinite List with take

-- ghci> let nums = [1..]  -- Infinite list of natural numbers
-- ghci> take 5 nums
-- [1,2,3,4,5]

-- Even though nums is infinite, Haskell only evaluates the first 5 elements because take 5 demands only those values.



-- Laziness and Short-Circuiting (|| and &&)
-- Lazy evaluation allows short-circuiting for logical operators.

-- Example: Short-Circuit ||

-- ghci> True || (10 `div` 0 == 0)  -- No division by zero error!
-- True
-- Since True || _ is always True, Haskell never evaluates (10 div 0 == 0), avoiding an error.







-- Laziness with undefined

-- undefined represents a computation that never produces a value.

-- Example: Forcing Evaluation (undefined in a list)

-- ghci> [1, 2, undefined, 4]  -- No error yet!
-- ghci> take 2 [1, 2, undefined, 4]
-- [1,2]
-- Since take 2 only needs the first two elements, undefined is never evaluated, so no error occurs.
-- If we ask for more elements:

-- ghci> take 4 [1, 2, undefined, 4]
-- ❌ This causes an error, because undefined is now accessed.


-- ghci> take 2 [1 , 2, 3, 4, undefined , 4]
-- [1,2]

-- ghci> take 5 [1 , 2, 3, 4, undefined , 4]
-- [1,2,3,4,*** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   undefined, called at <interactive>:8:22 in interactive:Ghci3




-- Laziness and seq (Forcing Evaluation)

-- ghci> let x = undefined
-- ghci> x `seq` "This won't print"
-- *** Exception: Prelude.undefined


