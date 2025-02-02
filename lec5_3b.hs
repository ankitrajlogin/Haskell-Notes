
{-# OPTIONS_GHC -Wall #-}

module IO where 


import qualified Control.Applicative


(>>) :: IO a -> IO b -> IO b 
(>>) = liftA2 (\ _ b -> b) 

liftA2 :: (a->b->c) -> IO a -> IO b -> IO c
liftA2  = Control.Applicative.liftA2 

-- ghci> getLine >> getLine 
-- 3asdf
-- asdf
-- "asdf"

program1 :: IO String 
program1 = 
    let 
        x = getLine 
        y = getLine 
    in 
        liftA2 (++) x y 



program2 :: IO String 
program2 = 
    let 
        x = getLine 
    in 
    fmap (\ y -> y ++ y) x


program2' :: IO String 
program2' = 
    fmap (\x -> x ++ x) getLine 


-- program3

program3 :: IO String 
program3 = 
    liftA2 (\x y -> y ++ x) getLine getLine 


-- ghci> program1
-- ankit
-- raj
-- "ankitraj"


-- ghci> program2
-- ankitraj
-- "ankitrajankitraj"


-- ghci> program2'
-- ankitraj
-- "ankitrajankitraj"

-- ghci> program3
-- ankit
-- raj
-- "rajankit"


