
module IO where 

import qualified Prelude 
import Prelude hiding ((>>)) 

import qualified Control.Applicative


(>>) :: IO a -> IO b -> IO b 
(>>) = (Prelude.>>)


-- ghci> :t (>>)
-- (>>) :: IO a -> IO b -> IO b

-- ghci> :t getLine >> getLine
-- getLine >> getLine :: IO String

-- ghci> getLine >> getLine
-- foo
-- gadf
-- "gadf"




-- ghci> :i IO 
-- type IO :: * -> *
-- newtype IO a
--   = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
--         -- Defined in ‘GHC.Types’
-- instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
-- instance Semigroup a => Semigroup (IO a) -- Defined in ‘GHC.Base’
-- instance Applicative IO -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance MonadFail IO -- Defined in ‘Control.Monad.Fail’
-- instance Monad IO -- Defined in ‘GHC.Base’



-- Program 1 
-- Read a line and reverse. 

-- ghci> fmap reverse getLine
-- hello
-- "olleh"

liftA2 :: (a ->b -> c) -> IO a -> IO b -> IO c
liftA2 = Control.Applicative.liftA2 

-- ghci> :t liftA2
-- liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c

-- ghci> liftA2 (++) getLine getLine
-- Ankit
-- Raj
-- "AnkitRaj"



