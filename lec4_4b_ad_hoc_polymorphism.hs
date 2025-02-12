
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Overloading where

import Prelude hiding ( Eq(..) , Ord(..)) 

-----------------------------------------------------
-- Overloading and Ad-hoc Polymorphism in Haskell
-----------------------------------------------------

-- In simpler words:


-- In Haskell, overloading refers to functions or operators that can work with different types based on type constraints. This is a form of ad-hoc polymorphism, which means that a function can operate on multiple types but requires explicit definitions or constraints for each type.


-----------------------------------------------------
-- 1. Ad-hoc Polymorphism (Overloading)
-----------------------------------------------------

-- Ad-hoc means specific to a situation.
-- Polymorphism means many forms.


-- Ad-hoc polymorphism means that a function or operator can work with different types, but the implementation depends on the type. In Haskell, this is achieved using type classes.


-- Example : 

-- Define the Printable type class
class Printable a where
    printVal :: a -> String  -- This defines a function to convert to a String

-- Instances for different types
instance Printable Int where
    printVal x = "Number: " ++ show x

instance Printable String where
    printVal x = "Word: " ++ x

instance Printable Bool where
    printVal True  = "Yes"
    printVal False = "No"

-- The main function to test everything
main2 :: IO ()
main2 = do
    print (printVal (42 :: Int))       -- Output: "Number: 42"
    print (printVal "Hello")           -- Output: "Word: Hello"
    print (printVal True)              -- Output: "Yes"


-- ghci> main2
-- "Number: 42"
-- "Word: Hello"
-- "Yes"


-- Understanding class Printable a where printVal :: a -> String
-- ---------------------------------------------------

-- This line of code defines a type class in Haskell, called Printable, which specifies a behavior that types can choose to implement. In simpler terms, it's like creating a contract that says, "If a type (e.g., Int, String, Bool) wants to be Printable, it must define how to turn itself into a String."


-- 2. Syntax Breakdown
-- ---------------------------------------------------
-- class Printable a where
-- class: This keyword is used to define a type class.
-- Printable: This is the name of the type class. It is used to group types that share a common behavior.
-- a: This is a type variable. It acts as a placeholder for any type that wants to be an instance of the Printable type class. For example, a can be replaced with Int, Bool, String, or any other type.
-- where: This keyword introduces the body of the type class, where you specify the functions that types must implement.
-- printVal :: a -> String
-- printVal: This is the function signature that defines what the Printable type class expects from types. It says:
-- "If a type is an instance of Printable, it must define how to convert a value of that type into a String."
-- a -> String: This means that printVal takes a value of type a (where a can be any type) and returns a String. So, for any type a that is an instance of Printable, you must implement printVal to convert that type to a String.



-- 3. Why Is This Useful?
-- -- ---------------------------------------------------
-- The Printable type class is useful because it allows you to generalize the behavior of converting various types into strings. Instead of writing separate functions for each type, you can write one generic function that works for many types, as long as those types are instances of Printable.

-- It’s a way to enable polymorphism in Haskell. Polymorphism means you can use the same function on different types without knowing exactly what types those are beforehand.

-- Example Use Case:
-- Let’s say you have a program where you need to print out different types of data (like numbers, strings, and booleans) in a consistent way.

-- You could define a type class Printable to specify that any type that is "printable" must implement a function to convert itself to a String. This way, you can write code that works with any printable type, whether it’s an Int, a String, or a Bool.


-- example 


-- 1. Defineing the type class (addable) 

class Addable a where
    add :: a -> a -> a  -- Function signature: takes two values and returns their sum

-- This defines a type class Addable, which requires any type implementing it to define how addition (add) should work.




-- 2. Making Int and Double Instances of Addable
-- Now, we implement Addable for Int and Double:

instance Addable Int where
    add x y = x + y  -- Integer addition
    

instance Addable Double where
    add x y = x + y  -- Floating point addition

-- Now, add can work on both Int and Double.




-- 3. Making a Custom Type (Vector2D) an Instance of Addable
-- Let's define a 2D vector type and make it "addable".

data Vector2D = Vector2D Double Double  -- A vector with x and y coordinates
    deriving Show 

instance Addable Vector2D where
    add (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)

-- This allows us to add two vectors using add.
-- Example: Vector2D 2 3 + Vector2D 4 1 = Vector2D 6 4.



-- If we used only a regular function, we would need separate functions for Double, Vector2D, etc.



main3 :: IO ()
main3 = do
    print (add (3 :: Int) (5 :: Int))         -- Output: 8
    print (add (2.5 :: Double) (4.3 :: Double))  -- Output: 6.8
    print (add (Vector2D 2 3) (Vector2D 4 1))    -- Output: Vector2D 6 4



-- ghci> main3
-- 8
-- 6.8
-- Vector2D 6.0 4.0






-- ---------------------------------------------------
-- 1. What is a Type Class?
-- -----------------------------------------------------

-- In Haskell, a type class is a way of defining generic behavior for types. It's a mechanism that allows you to write code that can work with multiple types without having to write separate functions for each type.


-- You can think of a type class as a set of rules that types can follow. If a type follows those rules (or implements the functions defined in the type class), it is said to be an instance of the type class.


class Eq a where 
    (==) :: a -> a -> Bool 
    (/=) :: a -> a -> Bool 
    x /= y = not ( x == y) 
    -- (/=) :: a -> a -> Bool 
    -- {-# MINIMAL (==) | (/=) #-}

instance Eq Bool where
    (==) :: Bool -> Bool -> Bool
    True == True = True 
    False == False = True
    _ == _ = False 


data Choice = Rock | Paper | Scissors 


instance Eq Choice where    
    (==) :: Choice -> Choice -> Bool
    Rock == Rock = True
    Paper == Paper = True
    Scissors == Scissors = True
    _ == _ = False

-- instance Eq Int where 
--     (==) :: Int -> Int -> Bool 
--     x == x = True 
--     _ == _ = False 



-- instance Eq Int where
--     (==) 0 0 = True
--     (==) 1 1 = True
--     (==) 2 2 = True
    -- and so on... (which is impractical)




instance Eq a => Eq [a] where 
    (==) :: [a] -> [a] -> Bool
    [] == [] = True
    (x : xs) == (y : ys) = x == y && xs == ys
    _  == _ = False  



-- >>> [Rock , Rock , Paper] == [Rock , Rock , Paper]
-- True

-- >>> [Rock , Paper, Paper] == [Rock , Rock , Paper]
-- False


-- as of now , we have no instance for integer . so that why we can not use that. 

-- >>> [1 , 2] == [1, 2]
-- Ambiguous type variable `a0_a26f1[tau:1]' arising from a use of `=='
-- prevents the constraint `(Eq a0_a26f1[tau:1])' from being solved.
-- Probable fix: use a type annotation to specify what `a0_a26f1[tau:1]' should be.
-- Potentially matching instances:
--   instance Eq Choice
--     -- Defined at /Users/ankit.raj/Desktop/pratice_haskell/Practice_lecture/lec4_4b_ad_hoc_polymorphism.hs:183:10
--   instance Eq Bool
--     -- Defined at /Users/ankit.raj/Desktop/pratice_haskell/Practice_lecture/lec4_4b_ad_hoc_polymorphism.hs:173:10
--   ...plus one other
--   (use -fprint-potential-instances to see them all)
-- In the expression: [1, 2] == [1, 2]
-- In an equation for `it_a26dA': it_a26dA = [1, 2] == [1, 2]
-- Ambiguous type variable `a0_a26f1[tau:1]' arising from the literal `1'
-- prevents the constraint `(Num a0_a26f1[tau:1])' from being solved.
-- Probable fix: use a type annotation to specify what `a0_a26f1[tau:1]' should be.
-- Potentially matching instances:
--   instance RealFloat a => Num (Complex a)
--     -- Defined in `Data.Complex'
--   instance forall k (a :: k). HasResolution a => Num (Fixed a)
--     -- Defined in `Data.Fixed'
--   ...plus 76 others
--   (use -fprint-potential-instances to see them all)
-- In the expression: 1
-- In the first argument of `(==)', namely `[1, 2]'
-- In the expression: [1, 2] == [1, 2]





-- defining new type of eq that use two different things and return true and false. 

class NewEq a b where
    comper :: a -> b  -> Bool 

instance NewEq Int Int where
    comper x y = False 
    


instance NewEq Integer String where
    comper x y = False 

instance NewEq String Integer where 
    comper x y = True 


-- >>> comper "Ankit" 3
-- True

-- >>> comper 2 "Ankit"
-- False
