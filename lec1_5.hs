
module Into where


-- If you want to use the name not, you can hide the not function from Prelude when importing it:
import Prelude hiding (not , (||) , elem)


not :: Bool -> Bool
not False = True
not True = False


-- similarly , this is how || define 
(||) :: Bool->Bool->Bool
False || y = y
True || y = True


-- Own list types
-- data Keyword
-- The data keyword is used to define a custom data type in Haskell.

-- ownList a
-- This is the name of the custom data type. It is parameterized by a type variable a.
-- Type Parameter a: This means ownList is a polymorphic type and can hold elements of any type (e.g., Int, Char, String, etc.).
-- For example:
-- ownList Int is a list of integers.
-- ownList Char is a list of characters.

-- Constructors
-- Nil:
-- Represents an empty list.
-- It is similar to the [] in Haskell's built-in list type.
-- Cons:
-- Represents a list with an element (a) and the rest of the list (ownList a).
-- It is similar to the (:) constructor in Haskell's built-in list type.


-- deriving Show
-- Automatically generates a Show instance for the ownList type.
-- The Show instance allows you to print values of ownList using show or directly in GHCi.
data OwnList a = Nil | Cons a (OwnList a)
    deriving Show

sampleList :: OwnList Int
sampleList = Cons 1 (Cons 2 (Cons 3 Nil))

sampleList2 :: OwnList Int 
sampleList2 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

-- similarily original list work
-- 1 : (2 : ( 3 : [])) ; 

-- ghci> 1 : (2 : ( 3 : [] ))
-- [1,2,3]


-- :i info about the variable. 

-- type Bool :: *
-- data Bool = False | True
--         -- Defined in ‘GHC.Types’
-- instance Bounded Bool -- Defined in ‘GHC.Enum’
-- instance Read Bool -- Defined in ‘GHC.Read’
-- instance Enum Bool -- Defined in ‘GHC.Enum’
-- instance Eq Bool -- Defined in ‘GHC.Classes’
-- instance Ord Bool -- Defined in ‘GHC.Classes’
-- instance Show Bool -- Defined in ‘GHC.Show’


-- type [] :: * -> *
-- data [] a = [] | a : [a]
--         -- Defined in ‘GHC.Types’
-- instance Traversable [] -- Defined in ‘Data.Traversable’
-- instance MonadFail [] -- Defined in ‘Control.Monad.Fail’
-- instance Monoid [a] -- Defined in ‘GHC.Base’
-- instance Semigroup [a] -- Defined in ‘GHC.Base’
-- instance Applicative [] -- Defined in ‘GHC.Base’
-- instance Foldable [] -- Defined in ‘Data.Foldable’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Monad [] -- Defined in ‘GHC.Base’
-- instance Read a => Read [a] -- Defined in ‘GHC.Read’
-- instance Ord a => Ord [a] -- Defined in ‘GHC.Classes’
-- instance Show a => Show [a] -- Defined in ‘GHC.Show’
-- instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’

infixr 2 || 

elem :: Eq a => a -> OwnList a ->Bool
elem x Nil = False
elem x (Cons y ys) = x == y || elem x ys

-- elem x (Cons y (Cons z ys)) = x==z
-- elem x (Cons y (Cons z (Cons a ys))) = x==a


-- ghci> elem 5 sampleList2
-- True

-- ghci> elem 6 sampleList2
-- False

-- directly difing finding funtion using list 
newelem :: Eq a => a -> [a] -> Bool
newelem x [] = False
newelem x (y : ys) = x==y || newelem x ys


 


-- Priorty of the operators. 

-- In Haskell, operators are assigned precedence and associativity to determine the order in which expressions are evaluated. 


-- Left-associative: Evaluated from left to right (default for most operators).
-- Right-associative: Evaluated from right to left (e.g., ^ for exponentiation).



-- Common Operator Precedences

-- # Operator Precedence and Associativity Table

-- | Operator                           | Meaning                              | Precedence | Associativity    |
-- |-------------------------------------|--------------------------------------|------------|------------------|
-- | `()`                                | Function application                 | 10         | Left-associative |
-- | `^, **`                             | Exponentiation                       | 8          | Right-associative|
-- | `*, /, div, mod`                    | Multiplication and division          | 7          | Left-associative |
-- | `+, -`                              | Addition and subtraction             | 6          | Left-associative |
-- | `==, /=, <, <=, >, >=`              | Comparison operators                 | 4          | Left-associative |
-- | `&&`                                | Logical AND                          | 3          | Left-associative |
-- | `` ` ``                             | Logical OR                           | 2          | Left-associative |
-- | `:`                                  | Cons (list construction)             | 5          | Right-associative|
-- | `=`                                  | Assignment (in some contexts)        | 0          | Left-associative |
-- | `->`                                | Arrow notation (function types)      | 0          | Right-associative|









