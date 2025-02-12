
-- ------------------------------------------
-- Parametric Polymorphism in Haskell
-- ------------------------------------------
-- Parametric polymorphism is a key feature of Haskell that allows functions and data types to operate on any type without depending on its specific structure. This makes code more reusable, generic, and type-safe.

-- Understanding Parametric Polymorphism
-- In Haskell, parametric polymorphism means that functions and types can be written in terms of type variables, which can represent any type. Unlike ad-hoc polymorphism (type classes) or subtype polymorphism (common in object-oriented languages), parametric polymorphism does not impose constraints on the type.



-- Example 1: Polymorphic Function
-- ------------------------------------------

identity :: a-> a 
identity x = x 

-- Here 
-- a is a type variable that can be any type.
-- The function identity works for Int, Bool, String, or any other type.


-- >>> identity 42       -- Returns 42
-- 42

-- >>> identity "hello"  -- Returns "hello"
-- "hello"

-- >>> identity True     -- Returns True
-- True




-- Example 2 : Polymorphic Data type 
-- ------------------------------------------

-- Haskell's list type is an example of parametric polymorphism

data List a = Empty | Cons a (List a)

-- Here:

-- a is a type variable.
-- List a can store elements of any type, such as List Int, List Bool, etc.
-- Haskell's built-in list type [a] is defined similarly.





-- Advantages of Parametric Polymorphism

-- Code Reusability – Functions can operate on any type.
-- Type Safety – Type checking ensures correctness without needing runtime checks.
-- Abstraction – Allows writing general, high-level code without worrying about specific types.



-- example 

fstTest :: (a , b) -> a 
fstTest (a , b ) = a

-- >>> fstTest (23 , "2342")
-- 23
