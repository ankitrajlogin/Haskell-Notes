

------------------------------------------------------
-- explain differnece between <$> and <$ in haskell. 
------------------------------------------------------


-- 1. <$> (Functor map)
------------------------------------------------------

-- <$> is the infix version of fmap, which applies a function to the contents of a functor.
-- It is used to map a function over a Functor, such as Maybe, List, or IO.


-- ghci> :i <$>
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
--         -- Defined in ‘Data.Functor’
-- infixl 4 <$>


-- >>> fmap (+1) (Just 5)   -- Just 6
-- Just 6

-- >>> (+1) <$> Just 5      -- Just 6
-- Just 6

-- Here, (+1) <$> Just 5 applies (+1) to the value inside Just, producing Just 6.




-- 2. <$ (Replace contents)
-- -------------------------------------
-- <$ replaces all values inside a functor with a given value, keeping the structure intact.
-- It is defined as:

-- -- (<$) :: a -> f b -> f a

-- Unlike <$>, it does not apply a function but instead replaces everything inside the functor with a fixed value.
-- Example:

-- ghci> :i <$
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   ...
--   (<$) :: a -> f b -> f a
--         -- Defined in ‘GHC.Base’
-- infixl 4 <$


-- >>> 42 <$ Just 5    -- Just 42
-- Just 42

-- >>> 42 <$ [1,2,3]   -- [42,42,42]
-- [42,42,42]

-- Here, 42 <$ Just 5 replaces 5 with 42, resulting in Just 42. Similarly, 42 <$ [1,2,3] produces [42,42,42].


-- Operator	                  Purpose	
-- <$>	     -->>             Maps a function over a functor	
-- <$	     -->>             Replaces all values in the functor with a constant

