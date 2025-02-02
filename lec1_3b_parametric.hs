

-- import Prelude hiding ((.))


-- -----------------------------------------
-- What Is Parametric Polymorphism?
-- -----------------------------------------

-- Parametric polymorphism is the ability to define functions or data types that can operate on values of any type without knowing the exact type in advance. The type is parameterized by one or more type variables, which makes the function or data structure generic.

-- This allows the same code to work with any type, as long as that type satisfies the constraints specified in the type signature (if any).




identity :: a -> a
identity x = x

-- In the above example:

-- a is a type variable that can represent any type.
-- The function identity takes a value of type a and returns a value of the same type a.


-- You can use this identity function with different types:


-- identity 5          -- Type: Int, Result: 5
-- identity "hello"    -- Type: String, Result: "hello"
-- identity [1, 2, 3]  -- Type: [Int], Result: [1, 2, 3]




-- ---------------------------------------
-- Parametric Polymorphism in Data Types
-- -----------------------------------------

-- Parametric polymorphism is also used in the definition of data types. For example, the Maybe type is polymorphic:

data Maybe a = Nothing | Just a

-- Here:

-- a is a type variable that can represent any type.
-- Maybe a is a type that can either be Nothing (representing an absence of a value) or Just a (containing a value of type a).





-- -----------------------------------------
-- Overloading 
-- -----------------------------------------

--  In Haskell, overloading refers to defining functions that can operate on different types, often using type classes. The most common example of overloading is seen with the (.) operator, which is used for function composition. The (.) operator in Haskell allows you to combine functions of compatible types to create a new function



-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f  g x  =  f(g(x))


f :: Int -> Int 
f x = x +1 

g :: Int -> Int 
g x = x *2 

h :: Int -> Int 
h = f .g  --- f (g (x)) 

-- >>> h 3 
-- 7



