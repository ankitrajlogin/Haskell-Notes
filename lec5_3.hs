
module IO where 

import qualified Prelude 
import Prelude hiding ((>>) , liftA2) 

import qualified Control.Applicative


-- (>>) :: Monad m => m a -> m b -> m b
-- ghci> :i (>>)
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   ...
--   (>>) :: m a -> m b -> m b
--   ...
--         -- Defined in ‘GHC.Base’
-- infixl 1 >>



(>>) :: IO a -> IO b -> IO b 
(>>) = (Prelude.>>)



-- Type Signature & Explanation
-- (>>) :: IO a -> IO b -> IO b
-------------------------------------------------
-- Breaking it Down
-- IO a → The first IO action that produces a value of type a (which will be discarded).
-- IO b → The second IO action that produces a value of type b.
-- Returns: IO b → The result of the second IO action.
-- So, (>>) action1 action2 means:

-- Execute action1 but ignore its result.
-- Then execute action2 and return its result.






-- ghci> :t getLine >> getLine
-- getLine >> getLine :: IO String

-- ghci> getLine >> getLine
-- foo
-- gadf
-- "gadf"


-- ghci> getLine>> getLine 
-- ankit
-- raj
-- "raj"





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





-- ghci> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b


-- ghci> fmap reverse getLine 
-- ankit
-- "tikna"


-- NOTE : 
-------------------------------------

-- fmap applies reverse inside IO
-- fmap :: (a -> b) -> IO a -> IO b













liftA2 :: (a ->b -> c) -> IO a -> IO b -> IO c
liftA2 = Control.Applicative.liftA2 

-- ghci> :t liftA2
-- liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c

-- ghci> liftA2 (++) getLine getLine
-- Ankit
-- Raj
-- "AnkitRaj"



