

-- The Maybe Type in Haskell
-- In Haskell, Maybe is a parametric data type used to represent computations that might fail or return nothing. It can either hold a value or represent an absence of a value. This type is often used as a safe alternative to null in other languages, where null can lead to runtime errors.


-- The Maybe type is defined as : 

-- data Maybe a = Nothing | Just a

-- Nothing: Represents the absence of a value (similar to null in other languages).
-- Just a: Represents a value of type a (i.e., a value of any type).


-- Maybe is used when you want to represent a value that might not exist or a computation that might fail. For example, if you try to look up a value in a dictionary, the result might either be Just value if the key is found, or Nothing if the key is absent.



-- Example function that uses Maybe
findFirstEven :: [Int] -> Maybe Int
findFirstEven [] = Nothing  -- No even numbers found
findFirstEven (x:xs)
  | even x    = Just x     -- Return the first even number
  | otherwise = findFirstEven xs  -- Recursively search the rest of the list



-------------------------------------
-- sJust and isNothing
-- -------------------------------------
-- These functions check whether a Maybe value contains a value (Just) or is empty (Nothing).

-- isJust :: Maybe a -> Bool    -- Returns True if the value is Just something
-- isNothing :: Maybe a -> Bool -- Returns True if the value is Nothing

-- isJust (Just 5)    -- True
-- isNothing (Nothing) -- True



-- -------------------------------------
-- Safe Divide. 
-- -------------------------------------

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing          -- Return Nothing when division by zero
safeDiv x y = Just (x `div` y) -- Otherwise, return the result inside Just





-- -------------------------------------
-- fromMaybe
-- -------------------------------------

-- The fromMaybe function extracts the value from a Maybe, providing a default if it's Nothing:


fromMaybe :: a -> Maybe a -> a
-- -- If it's Just value, return value, otherwise return the default value
fromMaybe defaultVal (Just x) = x
fromMaybe defaultVal Nothing  = defaultVal
 
-- >>> fromMaybe 0 (Just 5)  
-- 5

-- >>> fromMaybe 0 Nothing   -- Result: 0
-- 0





-- -------------------------------------
-- mapMaybe
-- -------------------------------------

-- You can also map over Maybe values using mapMaybe. It applies a function to the value inside Just and returns a new Maybe:

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
-- Transforms each element in the list, producing a list of Just values
mapMaybe f xs = [x | Just x <- map f xs]


doubleEven :: Int -> Maybe Int
doubleEven x
  | even x    = Just (x * 2)
  | otherwise = Nothing

-- >>> mapMaybe doubleEven [1, 2, 3, 4]  -- Result: [4, 8] (double only even numbers)
-- [4,8]
