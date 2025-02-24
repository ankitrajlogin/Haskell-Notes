

-- ghci> :i Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   {-# MINIMAL (>>=) #-}
--         -- Defined in ‘GHC.Base’
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monad ((,,) a b)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c)
--   -- Defined in ‘GHC.Base’
-- instance Monad ((->) r) -- Defined in ‘GHC.Base’
-- instance Monad IO -- Defined in ‘GHC.Base’
-- instance Monad Maybe -- Defined in ‘GHC.Base’
-- instance Monad Solo -- Defined in ‘GHC.Base’
-- instance Monad [] -- Defined in ‘GHC.Base’
-- instance Monad m => Monad (WrappedMonad m)
--   -- Defined in ‘Control.Applicative’
-- instance Monad (Either e) -- Defined in ‘Data.Either’





-- ghci> :i Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--   (*>) :: f a -> f b -> f b
--   (<*) :: f a -> f b -> f a
--   {-# MINIMAL pure, ((<*>) | liftA2) #-}
--         -- Defined in ‘GHC.Base’
-- instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Applicative ((,,) a b)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) =>
--          Applicative ((,,,) a b c)
--   -- Defined in ‘GHC.Base’
-- instance Applicative ((->) r) -- Defined in ‘GHC.Base’
-- instance Applicative IO -- Defined in ‘GHC.Base’
-- instance Applicative Maybe -- Defined in ‘GHC.Base’
-- instance Applicative Solo -- Defined in ‘GHC.Base’
-- instance Applicative [] -- Defined in ‘GHC.Base’
-- instance Control.Arrow.Arrow a => Applicative (WrappedArrow a b)
--   -- Defined in ‘Control.Applicative’
-- instance Monad m => Applicative (WrappedMonad m)
--   -- Defined in ‘Control.Applicative’
-- instance Applicative ZipList -- Defined in ‘Control.Applicative’
-- instance Applicative (Either e) -- Defined in ‘Data.Either’
-- instance Monoid m => Applicative (Const m)
--   -- Defined in ‘Data.Functor.Const’



-- ghci> :i Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--         -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Functor Solo -- Defined in ‘GHC.Base’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Control.Arrow.Arrow a => Functor (WrappedArrow a b)
--   -- Defined in ‘Control.Applicative’
-- instance Monad m => Functor (WrappedMonad m)
--   -- Defined in ‘Control.Applicative’
-- instance Functor ZipList -- Defined in ‘Control.Applicative’
-- instance Functor (Either a) -- Defined in ‘Data.Either’
-- instance Functor (Const m) -- Defined in ‘Data.Functor.Const’
-- instance Functor (Map k) -- Defined in ‘Data.Map.Internal’


import Control.Monad (liftM , ap)

newtype Counter a = MkCounter { runCounter :: Int -> (a , Int) }



(>>>>=) :: Counter a -> (a -> Counter b ) -> Counter b 
comp >>>>= rest = 
    MkCounter ( \ c0 ->
        let
            (a , c1) = runCounter comp c0 
        in 
            runCounter (rest a) c1 
    
    )


returnCounter :: a -> Counter a 
returnCounter a = MkCounter (\c -> (a , c)) 


-----------------------------------------------------------------
-- Making Counter an Instance of Functor, Applicative, and Monad
-----------------------------------------------------------------


instance Functor Counter where 
    fmap = liftM


-- It takes a function f :: a -> b and applies it to the value inside Counter a.





instance Applicative Counter where
    pure = returnCounter
    (<*>) = undefined 


-- pure is just returnCounter, which wraps a value in a Counter without changing the counter.
-- (<*>) (applicative apply) isn't defined, but it would allow applying a Counter (a -> b) to Counter a.


instance Monad Counter where 
    (>>=) = (>>>>=) 


-- This makes Counter a monad, enabling do-notation and sequential operations.





-- fmapIO :: (a -> b) -> IO a -> IO b 



-- liftM :: Monad m => (t -> b) -> m t -> m b
-- liftM f comp =
--     comp >>= \a -> pure (f a)


-- This is already exist in the name of liftM

-- ghci> :t Control.Monad.liftM
-- Control.Monad.liftM :: Monad m => (a1 -> r) -> m a1 -> m r





