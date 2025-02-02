
-- data I0 a -- abstract
-- The type of plans to perform effects that ultimately yield an a .
-- • Evaluation does not trigger the actual effects. It will at most evaluate the plan.
-- • Execution triggers the actual effects. Executing a plan is not possible from within a Haskell program.


module Main where 


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


-- main :: IO () 
-- main = putStrLn "Hello World!"


main :: IO() 
main = writeFile "lec5_2.txt" "Hello\n\tworld!\n"

-- ghci> :t getLine 
-- getLine :: IO String

-- ghci> getLine 
-- foo
-- "foo"

-- ghci> readFile "test.txt"
-- "Hello\n\tworld!\n"


