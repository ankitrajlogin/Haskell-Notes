
module Into where


-- In Haskell, a module declaration like:

-- Declares a module named Into

-- This means all definitions (functions, types, etc.) in this file belong to the Into module.
-- Other modules can import it using import Into.


-- ghci> take 2 [1,24,5,3]
-- [1,24]

-- ghci> not True
-- False

-- ghci> min 3 4
-- 3

-- ghci> take 2 [1,3,9 , 27 , 81]
-- [1,3]


-- ghci> 2 : [3 , 4 , 5]
-- [2,3,4,5]

-- : is the Cons Operator
-- : (called cons) is a constructor that adds an element to the front of a list.
-- It takes an element and a list and returns a new list with that element prepended.



-- Equivalent Expansions
-- The list [2, 3, 4, 5] is actually syntactic sugar for:

-- 2 : (3 : (4 : (5 : [])))

-- 5 : [] → [5]
-- 4 : [5] → [4,5]
-- 3 : [4,5] → [3,4,5]
-- 2 : [3,4,5] → [2,3,4,5]



-----------------------
-- Operation in between ---------------------
-----------------------

-- In Haskell, all operators are actually functions.

-- + is a function of type:

-- haskell
-- (+) :: Num a => a -> a -> a
-- This means it takes two numbers and returns their sum.

-- When you write 4 + 5, it's syntactic sugar for (+) 4 5.

-- You can use any operator in prefix notation by surrounding it with parentheses.

-- ghci> 4 + 5
-- 9
-- ghci> (+) 4 5
-- 9

-- ghci> 2 : [3,4,5]
-- [2,3,4,5]

-- ghci> (:) 2 [3, 4, 5]
-- [2,3,4,5]




-- min - --------'

-- min as a Regular Function (min 2 4)
-- min is a built-in function that takes two arguments and returns the smaller one.
-- Its type signature is:
-- min :: Ord a => a -> a -> a

-- This means min works on any type that has an ordering (Ord typeclass).
-- When you call:
-- min 2 4
-- Haskell applies min to 2 and 4, returning 2.

-- min Used as an Infix Operator (2 \min` 4`)
-- In Haskell, any binary function (a function taking two arguments) can be used as an infix operator by enclosing it in backticks (`).




-- ghci> :i filter
-- filter :: (a -> Bool) -> [a] -> [a]     -- Defined in ‘GHC.List’


-- ghci> :i odd
-- odd :: Integral a => a -> Bool  -- Defined in ‘GHC.Real’


-- as filter function need and function that take each value of list and return a bool and list. 

-- ghci> filter odd [3,4,5,6,78]
-- [3,5]





-------------------------------------------
-- Lambda function 
-------------------------------------------


-- ghci> (\ x -> x +3 ) 4
-- 7

-- ghci> (\x y -> x+y ) 4 5 
-- 9

-- ghci> (\list n -> take n ( reverse list)) "hello" 3
-- "oll"



-- ghci> replicate 3 'x'
-- "xxx"