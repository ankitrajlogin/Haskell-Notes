{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (sum , (||) , length, map , filter , any) 

length :: [a] -> Int
length [] = 0 
length (_ : xs) = 1 + length xs 

addTwo :: Int -> Int
addTwo x = x+2 

map :: (a->b) -> [a]-> [b] 
map _ [] = [] 
map f (x : xs) = f x : map f xs




filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = [] 
filter p (x: xs) = 
    case p x of 
        False -> filter p xs
        True -> x : filter p xs 


filter2 :: (a->Bool) -> [a] -> [a]
filter2 _ [] = [] 
filter2 p (x : xs) = 
    if p x
        then x : filter2 p xs
        else filter p xs 



-- if p x then x : filter p xs else filter p xs

-- filter p (x : xs) 
-- | p x = x : filter p xs 
-- | otherwise = filter p xs

-- | - we use guards (|) to decide what to do for each element 
-- case 1 : if p x is true then --> x : filter p xs 
-- otherwise - filter p xs 



-- TODO 
-- By pattern matching, redefine the function any :: (a -> Bool) -> [a] -> Bool from the Prelude that checks whether a given list contains at least one element that passes the given test.

any :: (a -> Bool) -> [a] -> Bool 
any _f [] = False
any f (x : xs) = if f x then True else any f xs

-- any f (x : xs) = f x || any f xs




-- >>> map (\x -> x +5) [1..50]
-- [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55]


-- >>> map (+2) [1..50]
-- [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52]


-- >>> map (2+) [1..50]
-- [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52]


-- >>>  map (/2) [1..50]
-- [0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5,9.0,9.5,10.0,10.5,11.0,11.5,12.0,12.5,13.0,13.5,14.0,14.5,15.0,15.5,16.0,16.5,17.0,17.5,18.0,18.5,19.0,19.5,20.0,20.5,21.0,21.5,22.0,22.5,23.0,23.5,24.0,24.5,25.0]


-- >>> map (2/) [1..50]
-- [2.0,1.0,0.6666666666666666,0.5,0.4,0.3333333333333333,0.2857142857142857,0.25,0.2222222222222222,0.2,0.18181818181818182,0.16666666666666666,0.15384615384615385,0.14285714285714285,0.13333333333333333,0.125,0.11764705882352941,0.1111111111111111,0.10526315789473684,0.1,9.523809523809523e-2,9.090909090909091e-2,8.695652173913043e-2,8.333333333333333e-2,8.0e-2,7.692307692307693e-2,7.407407407407407e-2,7.142857142857142e-2,6.896551724137931e-2,6.666666666666667e-2,6.451612903225806e-2,6.25e-2,6.060606060606061e-2,5.8823529411764705e-2,5.714285714285714e-2,5.555555555555555e-2,5.405405405405406e-2,5.263157894736842e-2,5.128205128205128e-2,5.0e-2,4.878048780487805e-2,4.7619047619047616e-2,4.6511627906976744e-2,4.5454545454545456e-2,4.4444444444444446e-2,4.3478260869565216e-2,4.25531914893617e-2,4.1666666666666664e-2,4.081632653061224e-2,4.0e-2]



-- >>> map (min 2) [1..5]
-- [1,2,2,2,2]


-- >>> map (2-) [1..5]
-- [1,0,-1,-2,-3]


-- >>> map  (-2) [1..5]
-- Could not deduce (Num a0_a5e1Z[tau:1])
-- from the context: (Num a_a5e1T[sk:1],
--                    Num (a_a5e1T[sk:1] -> b_a5e1U[sk:1]), Enum a_a5e1T[sk:1])
--   bound by the inferred type for `it_a5e0b':
--              forall {a} {b}. (Num a, Num (a -> b), Enum a) => [b]
--   at /Users/ankit.raj/Desktop/pratice_haskell/Practice_lecture/lec2_6_mapping_filter.hs:89:2-17
-- The type variable `a0_a5e1Z[tau:1]' is ambiguous
-- Potentially matching instances:
--   instance RealFloat a => Num (Complex a)
--     -- Defined in `Data.Complex'
--   instance forall k (a :: k). HasResolution a => Num (Fixed a)
--     -- Defined in `Data.Fixed'
--   ...plus 76 others
--   (use -fprint-potential-instances to see them all)
-- In the ambiguity check for the inferred type for `it_a5e0b'
-- To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
-- When checking the inferred type
--   it_a5e0b :: forall {a} {b}. (Num a, Num (a -> b), Enum a) => [b]




-- >>> map (+(-2)) [1..5] 
-- [-1,0,1,2,3]
