
module Monads where 

import Data.Map.Strict as M


table  :: M.Map Int Int
table = 
    M.fromList 
    [ (1,2) , 
    (2 , 4) , 
    ( 3, 6) , 
    (4, 8 ) , 
    ( 5, 10 ) , 
    ( 6 , 12) , 
    (8 , 16) , 
    (10 , 20 ) , 
    (15 , 30) , 
    (16 , 32) , 
    ( 20 , 40 ) , 
    (32 , 64) 
    ]


-- Task:
-- Starting from a given integer, we want to perform three successive lookups in our table. (By successive, we mean that the result of one lookup serves as an input for the next.) Then we return the final result incremented by one.

-- If any of the lookups fail, then the whole operation should fail.

lookupTimes :: Int -> Int -> Maybe Int
lookupTimes i0 times = 
    case M.lookup i0 table of 
        Nothing -> Nothing 
        Just i1 -> 
            if times <= 0 
                then Just (i1 + 1)  
                else lookupTimes i1 (times - 1)  

-- ghci> lookupTimes 2 4
-- Just 65



threeLookups :: Int -> Maybe Int
threeLookups i0 = 
    case M.lookup i0 table of 
        Nothing -> Nothing 
        Just i1 -> 
            case M.lookup i1 table of 
            Nothing -> Nothing 
            Just i2 -> 
                case M.lookup i2 table of 
                Nothing -> Nothing 
                Just i3 -> Just (i3+1)



-- ghci> threeLookups 2
-- Just 17

-- ghci> threeLookups 5
-- Just 41

-- ghci> threeLookups 7
-- Nothing





fun :: Maybe a -> (a -> Maybe b) -> Maybe b
fun comp rest = 
    case comp of 
        Nothing -> Nothing  
        Just x -> rest x  

-- (>>=) :: IO a -> (a -> IO b) -> IO b 

(>>>=) :: Maybe a -> (a-> Maybe b) -> Maybe b 
comp >>>= rest = 
    case comp of
        Nothing -> Nothing 
        Just x -> rest x 



threeLookups2 :: Int -> Maybe Int
threeLookups2 i0 = 
    M.lookup i0 table >>>= \i1 -> 
    M.lookup i1 table >>>= \i2 -> 
    M.lookup i2 table >>>= \i3 ->
    Just(i3 +1) 






