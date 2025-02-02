
module Deriving where 



data Choice = Rock | Paper | Scissors 
    deriving (Eq , Ord , Show , Read , Enum , Bounded) 

 

 -- Before using deriving . 

--  ghci> Rock
-- <interactive>:2:1: error:
--     • No instance for (Show Choice) arising from a use of ‘print’
--     • In a stmt of an interactive GHCi command: print it

-- ghci> Rock
-- Rock

-- ghci> Rock == Rock
-- True

-- ghci> Rock == Paper
-- False

-- ghci> Rock <= Paper
-- True

-- ghci> Rock < Paper
-- True

-- ghci> Rock > Paper
-- False


-- ghci> :i Bool
-- type Bool :: *
-- data Bool = False | True
--         -- Defined in ‘GHC.Types’
-- instance Bounded Bool -- Defined in ‘GHC.Enum’
-- instance Read Bool -- Defined in ‘GHC.Read’
-- instance Enum Bool -- Defined in ‘GHC.Enum’
-- instance Eq Bool -- Defined in ‘GHC.Classes’
-- instance Ord Bool -- Defined in ‘GHC.Classes’
-- instance Show Bool -- Defined in ‘GHC.Show’


-- ghci> Nothing < Just 'x'
-- True

-- ghci> read "Rock" :: Choice
-- Rock

-- ghci> minBound :: Choice
-- Rock

-- ghci> maxBound :: Choice
-- Scissors

-- ghci> [minBound..] :: [Choice]
-- [Rock,Paper,Scissors]




---- Tree ----------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Eq , Ord , Show , Read) 


-- For Tree, there is no natural way to define what the "next" or "previous" tree is. Consider:

-- What is the "successor" of a Leaf a?
-- What is the "successor" of a Node (Tree a) (Tree a)?


-- Why Tree Cannot Derive Bounded
-- For Tree, there’s no obvious "minimum" or "maximum" value. This is particularly true because Tree is recursive:

-- A Tree may consist of Node or Leaf, which themselves may recursively contain more Trees.
-- There is no clear way to define minBound or maxBound for arbitrary Tree structures.

tree3 :: Tree String
tree3 = Node (Leaf "hello") (Node (Leaf "world") (Leaf "Haskell"))

--        Node
--       /    \
--   "hello"   Node
--            /    \
--        "world"  "Haskell"




-- ghci> Leaf 'x' == Leaf 'y'
-- False

-- ghci> compare (Node (Leaf 3) (Leaf 4)) (Node (Leaf 3) (Leaf 4))
-- EQ


-- ghci> :i Tree
-- type Tree :: * -> *
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--         -- Defined at lec4_7.hs:69:1
-- instance Eq a => Eq (Tree a) -- Defined at lec4_7.hs:70:15
-- instance Ord a => Ord (Tree a) -- Defined at lec4_7.hs:70:20
-- instance Read a => Read (Tree a) -- Defined at lec4_7.hs:70:33
-- instance Show a => Show (Tree a) -- Defined at lec4_7.hs:70:26

-- these are the instance for deriving class. 



-- Function Making -----------


data WrappedIntFunction = MKFunction (Int ->Int)
    -- deriving (Eq , Ord, Show , Read)

-- 1. Eq:
-- The Eq typeclass requires an implementation of (==) to compare two values for equality.
-- However, functions cannot be directly compared for equality in Haskell because it's generally undecidable whether two functions are equal (due to infinite possible inputs and undecidable equivalence in the general case).


-- 2. Ord:
-- The Ord typeclass requires functions like (<=) for ordering. There is no natural or meaningful way to "order" functions.

-- 3. Show:
-- The Show typeclass requires converting values to String. While we can show the structure of WrappedIntFunction, we cannot display the function itself in a meaningful way (other than a placeholder like "<function>").

-- 4. Read:
-- The Read typeclass requires parsing a String back into a value. Functions cannot be reconstructed from strings in Haskell, so Read cannot be derived.



-- ghci> :i WrappedIntFunction
-- type WrappedIntFunction :: *
-- data WrappedIntFunction = MKFunction (Int -> Int)
--         -- Defined at lec4_7.hs:106:1


