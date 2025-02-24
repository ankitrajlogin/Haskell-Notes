
module Monads where 

import Data.List (map) 

import Control.Applicative
import Data.Map.Strict as M 

import Prelude hiding (mapM, sequence)


data BinTree a = 
    Bin (BinTree a) a (BinTree a)
    | Empty 
    deriving (Eq , Show) 


example :: BinTree Char 
example =
    Bin
        (Bin
            (Bin Empty 'q' Empty)
            'a'
            Empty
        )
        'x'
        (Bin
            (Bin Empty 'h' Empty)
            'b'
            (Bin Empty 'p' Empty)
        )




tree5 :: BinTree Char
tree5 = Bin (Bin Empty 'A' Empty) 'B' (Bin Empty 'C' Empty)

tree6 :: BinTree Char
tree6 = Bin 
            (Bin 
                (Bin Empty 'A' Empty) 
                'B' 
                (Bin Empty 'C' Empty)
            ) 
            'D' 
            (Bin 
                (Bin Empty 'E' Empty) 
                'F' 
                (Bin 
                    (Bin Empty 'G' Empty) 
                    'H' 
                    (Bin Empty 'I' Empty)
                )
            )


labelTreeOrg :: BinTree a -> BinTree (a, Int)
labelTreeOrg tree = fst (labelTreeAuxOrg tree 1)

labelTreeAuxOrg :: BinTree a -> Int -> (BinTree (a, Int), Int)
labelTreeAuxOrg Empty c0 = (Empty, c0)
labelTreeAuxOrg (Bin l x r) c0 = 
    let 
        (newl, c1) = labelTreeAuxOrg l c0 
        c2 = c1 + 1 
        (newr, c3) = labelTreeAuxOrg r c2 
    in 
        (Bin newl (x, c1) newr, c3)



-------------------------------------------------------
-- Creating a counter  datatype
-------------------------------------------------------

newtype Counter a = MkCounter { runCounter :: Int -> (a , Int) }


----------------------------------------------------------
----------------------------------------------------------
-- NOTE : we can also write this as : 

newtype Counter2 a = MkCounter2 (Int -> ( a , Int)) 

runCounter2 :: Counter2 a -> (Int -> (a , Int)) 
runCounter2 (MkCounter2 f) = f 

-------------------------------------------------------------------
-------------------------------------------------------------------



-- ghci> :t MkCounter
-- MkCounter :: (Int -> (a, Int)) -> Counter a

-- What is newtype?
-- newtype is used to define a new type that wraps an existing type.
-- It ensures zero runtime overhead (unlike data).
-- You can only have one constructor.


-- MkCounter is the constructor that creates a Counter a from a function.
-- runCounter is a getter function that extracts the underlying function.


-- Here, MkCounter is just a wrapper around the function Int -> (a, Int), but we don't have an easy way to extract it.

-- { runCounter :: Int -> (a, Int) } creates a named field.
-- This automatically generates a function runCounter that extracts the function inside MkCounter.




-- Without runCounter, We Must Manually Extract the Function
-- If we used:
-- newtype Counter a = MkCounter (Int -> (a, Int))




labelTree :: BinTree a -> BinTree (a, Int)
labelTree tree = 
    fst (runCounter (labelTreeAux tree) 1) 



stepCounter :: Counter Int 
stepCounter = MkCounter (\ c -> (c , c + 1)) 


labelTreeAux :: BinTree a -> Counter (BinTree (a, Int))
labelTreeAux Empty = MkCounter (\ c0 -> (Empty, c0))
labelTreeAux (Bin l x r) = MkCounter (\ c0 -> 
    let 
        (newl , c1) = runCounter (labelTreeAux l) c0 
        (lx , c2) = runCounter stepCounter c1 
        (newr , c3) = runCounter (labelTreeAux r) c2 
    in 
        (Bin newl (x, lx) newr, c3))


-- lx is same as c1 . 
-- stepCounter = MkCounter (\c -> (c, c + 1))
-- It takes a counter c and returns (c, c+1), meaning:
-- The current count is used as the label.
-- The counter is incremented for the next use.

-------------------------------------------------------------------
-- Step-by-Step Breakdown
-------------------------------------------------------------------

-- 1. Traverse the left subtree first:

-- (newl, c1) = runCounter (labelTreeAux l) c0
-- labelTreeAux l recursively labels the left subtree.
-- It starts with counter c0 and produces a new left subtree newl with an updated counter c1.


-- 2. Assign a label to the current node:

-- (lx, c2) = runCounter stepCounter c1
-- stepCounter gives the current counter value lx as the label.
-- The counter is updated to c2.


-- 3. Traverse the right subtree:

-- (newr, c3) = runCounter (labelTreeAux r) c2
-- Recursively labels the right subtree, using c2 as the starting counter.
-- Produces a new right subtree newr and updates the counter to c3.


-- 4.  Return the labeled node:

-- (Bin newl (x, lx) newr, c3)
-- The new tree node contains:
-- The left subtree (newl).
-- The original value x along with the assigned label lx.
-- The right subtree (newr).
-- Returns the final counter c3.










-- ghci> labelTree tree5
-- Bin (Bin Empty ('A',1) Empty) ('B',2) (Bin Empty ('C',3) Empty)
