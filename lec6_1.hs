

-- Finite Maps --------
----------------------


module Monads where 


import Data.Map.Strict as M


-- ghci> :t Data.Map.Strict.lookup
-- Data.Map.Strict.lookup :: Ord k => k -> Map k a -> Maybe a

-- ghci> :t M.lookup
-- M.lookup :: Ord k => k -> Map k a -> Maybe a


example :: M.Map Int Int
example = 
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


-- ghci> example
-- fromList [(1,2),(2,4)]


-- ghci> M.singleton 5 10
-- fromList [(5,10)]

-- ghci> M.insert 5 10 example 
-- fromList [(1,2),(2,4),(5,10)]


-- ghci> M.lookup 7 example
-- Nothing

-- ghci> M.lookup 4 example 
-- Just 8


