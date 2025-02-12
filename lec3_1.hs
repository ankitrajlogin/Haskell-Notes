

{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (reverse)





reverse :: [a] -> [a]
reverse [] = [] 
reverse (x : xs) = reverse xs ++ [x]

-- Why It Is Slow ( Lazy evaluation )
-- The slowness comes from the use of ++ (list concatenation), which has a time complexity of O(n), where n is the length of the first list in the concatenation.

-- Breaking Down the Cost:
-- When reverse is called on a list of length n, it must append the current element x to the end of the reversed result (reverse xs).
-- Each recursive step involves a call to ++, which traverses the entire reversed list built so far.
-- The total cost is O(1 + 2 + 3 + ... + n) = O(n²).




-- How to Make It Faster
-- The inefficiency can be avoided by avoiding repeated concatenation. Instead of appending elements to the end of a list, we can prepend elements to the front of an accumulator using : (cons operator), which is O(1).





reverseAux :: [a] -> [a] -> [a]
reverseAux acc [] = acc 
reverseAux acc (x : xs) = reverseAux (x : acc) xs 


reverse_fast :: [a] -> [a] 
reverse_fast ax = reverseAux [] ax 

-- or reverse_fast = reverseAux []  

-- Explanation:
-- Accumulator (acc): Instead of appending to the list after each recursive call, we maintain an accumulator that collects the reversed elements as we go through the original list.
-- Tail Recursion: This version of reverse is tail-recursive, meaning the function can be optimized by the compiler to avoid building up a call stack. Each recursive call processes one element and passes the result forward in the accumulator.
-- Linear Time: This approach runs in O(n) time because there’s no repeated traversal of the list. Each element is added to the accumulator exactly once.



-- int this , first list is accumulator , which appending the value from the original one and appending.  
reverseAux2 :: [a] -> [a] -> [a]
-- when list is empty , all value in acc is equal 
reverseAux2 acc [] = acc 
reverseAux2 acc (x : xs) = reverseAux2 (x : acc) xs 
-- in this , we taking first value from the list and adding in the accumulator , using this, we finally remove from original list and adding in accumulator. 



