module Interaction where 


import System.IO.Unsafe

-- ghci> import System.IO.Unsafe 
-- ghci> :t unsafePerformIO
-- unsafePerformIO :: IO a -> a


---------------------------------------------------------
-- NOTE : 
---------------------------------------------------------
-- (>>=) :: IO a -> (a -> IO b) -> IO b 
-- liftA2 :: (a -> b -> c) ->  IO a -> IO b -> IO c 
-- fmap :: (a -> b) -> IO a -> IO b 

-- there 3 give the access of IO value but final result is also IO. 





-------------------------------------------------------
-- Question is : 
-- How we can get IO int -> int , IO char -> char , IO String -> IO String , etc.
-- Vise Versa. 
-------------------------------------------------------


-- ghci> :t unsafePerformIO
-- unsafePerformIO :: IO a -> a

-- but we should not use unsafePerformIO because it can give unusual behaviour. 


