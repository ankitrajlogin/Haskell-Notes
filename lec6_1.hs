

-- Finite Maps --------
----------------------


module Monads where 


import qualified Data.Map.Strict as M


-- ghci> :t Data.Map.Strict.lookup
-- Data.Map.Strict.lookup :: Ord k => k -> Map k a -> Maybe a

-- ghci> :t M.lookup
-- M.lookup :: Ord k => k -> Map k a -> Maybe a





-- M.Map → Refers to the Map type from the Data.Map.Strict module.
-- Int → Represents the type of keys.
-- Int → Represents the type of values.

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






---------------------------------------------------------------
-- lookup in Haskell: Everything You Need to Know
---------------------------------------------------------------

-- lookup is a function used to retrieve the value associated with a given key in associative structures like lists of key-value pairs or maps (Data.Map). If the key exists, it returns Just value; otherwise, it returns Nothing.


-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- a → Key type (must be comparable with Eq).
-- b → Value type.
-- [(a, b)] → A list of key-value pairs (an association list).
-- Maybe b → Returns Just value if key is found, otherwise Nothing.



main2 :: IO ()
main2 = do
    let myList = [(1, "one"), (2, "two"), (3, "three")]
    print $ lookup 2 myList  -- Just "two"
    print $ lookup 4 myList  -- Nothing


-- ghci> main2
-- Just "two"
-- Nothing



myLookup :: Eq a => a -> [(a,b)] -> Maybe b 
myLookup _ [] = Nothing 
myLookup key ((k ,v) : xs) 
    | key == k = Just v 
    | otherwise = myLookup key xs 


main3 = do
    let myList = [(1, "one"), (2, "two"), (3, "three")]
    print $ myLookup 2 myList  -- Just "two"
    print $ myLookup 4 myList  -- Nothing

-- ghci> main3
-- Just "two"
-- Nothing




--------------------------------------------------
-- Final NOTE 
--------------------------------------------------

-- lookup is simple but linear in performance (good for small lists).
-- M.lookup in Data.Map.Strict is faster (O(log n)) and ideal for large datasets.
-- Always handle Maybe values properly to avoid runtime errors.