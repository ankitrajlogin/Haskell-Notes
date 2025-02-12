{-# OPTIONS_GHC -Wall #-}
module Functions where

import Prelude hiding (zip , zipWith , lookup)


example :: [(Int, String)]
example =
    [
        (1, "Frodo"),
        (2, "Bilbo"),
        (3, "Dandalf"),
        (11, "Lego") , 
        (3, "salman")
    ]



-- ghci> :t error
-- error :: GHC.Stack.Types.HasCallStack => [Char] -> a

lookup :: Eq key => key -> [(key , val)] -> val
lookup _key [] = error "key not found"
lookup key ((key' , val) : table) = if key == key' then val  else lookup key table


-- ghci> lookup 4 example 
-- "*** Exception: key not found
-- CallStack (from HasCallStack):
--   error, called at lec2_11.hs:23:18 in main:Functions

-- key not found , then it giving error and crash the file. 





-- other types to write. 
lookup' :: Eq key => key -> [(key, val)]  -> val 
lookup' _key [] = error "key not found"
lookup' key ((key' , val) : table) 
    | key == key' = val 
    | otherwise = lookup key table

-- ghci> lookup' 711 example
-- "*** Exception: key not found
-- CallStack (from HasCallStack):
--   error, called at lec2_11.hs:23:18 in main:Functions

-- When key not found , it gives error and sytem get crash. 
-- So we have to handle this. 

