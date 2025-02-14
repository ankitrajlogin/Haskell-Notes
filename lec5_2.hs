
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





-----------------------------------
--  IO() means must retunr IO () 
-----------------------------------

-- main2 :: IO() 
-- main2 = "Ankit" 

-- main must be an IO action because it handles input/output.
-- A String is not an IO action, so "Ankit" alone is invalid.
-- Use putStrLn to turn a String into an IO () action.



main2 :: IO () 
main2 = putStrLn "Hello World" 



-- ghci> :i () 
-- type () :: *
-- data () = ()
--         -- Defined in ‘GHC.Tuple.Prim’
-- instance Monoid () -- Defined in ‘GHC.Base’
-- instance Semigroup () -- Defined in ‘GHC.Base’
-- instance Bounded () -- Defined in ‘GHC.Enum’
-- instance Read () -- Defined in ‘GHC.Read’
-- instance Enum () -- Defined in ‘GHC.Enum’
-- instance Show () -- Defined in ‘GHC.Show’
-- instance Eq () -- Defined in ‘GHC.Classes’
-- instance Ord () -- Defined in ‘GHC.Classes’



