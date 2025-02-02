
module Into where



-- ghci> :t words
-- words :: String -> [String]


-- ghci> words "hello"
-- ["hello"]


-- • Every expression (and subexpression) has a type.
-- • Types are checked statically.
-- • Types can be inferred.



data Nat = Zero | Succ Nat  -- Defines a type for natural numbers

-- Here, Zero and Succ n are values, but Nat is a type.

-- the :: symbol read "is of type"

-- Types are different from expressions. you cannot use a type as an expression. 


-- ghci> :t reverse 
-- reverse :: [a] -> [a]

-- ghci> :t take 
-- take :: Int -> [a] -> [a]


-- a -> [a] -> [a] means: "Takes an element and a list, and returns a new list".

-- Another view : A function that takes an a and return another function , which then expects a [a] and return [a]

-- ghci> :t take 
-- take :: Int -> [a] -> [a]
-- ghci> :t take
-- take :: Int -> [a] -> [a]
-- ghci> :t take 2 
-- take 2 :: [a] -> [a]

-- ghci> :t take 2 "carrying"
-- take 2 "carrying" :: [Char]

-- ghci> :t words 
-- words :: String -> [String]

-- ghci> :doc words
-- words :: String -> [String]
--         -- Identifier defined in ‘base-4.17.2.1:Data.OldList’
-- -- | 'words' breaks a string up into a list of words, which were delimited
-- -- by white space.
--
-- >>> words "Lorem ipsum\ndolor"
-- ["Lorem","ipsum","dolor"]

-- ghci> "raj"  : (words "rahul")
-- ["raj","rahul"]

-- ghci> words "345"
-- ["345"]


-- ghci> words "hello everyone my name is ankit raj"
-- ["hello","everyone","my","name","is","ankit","raj"]


-- >>> words "hello world"
-- ["hello","world"]

-- >>> :t replicate
-- replicate :: Int -> a -> [a]

-- >>> replicate 3 "run"
-- ["run","run","run"]


-- >>> :t take 2 "currying" 
-- take 2 "currying" :: [Char]

--  it is list of char type because we have to not supply any things to it and it just return list of char type. 


-- >>>  map (take 2) [[1,2,3] ,[2] , [4,5,6]]
-- [[1,2],[2],[4,5]]


-- >>> map (\ x -> take 2 x) [[1,2,3] , [2] , [ 4, 5 ,6]]
-- [[1,2],[2],[4,5]]

-- This is the same operation, but written with a lambda function instead of direct function application.

-- map (take 2) is shorter and cleaner because take 2 is already a function.
-- The lambda version (map (\x -> take 2 x)) is explicit, but redundant.



-- Built-in Typeclasses in Haskell


-- Eq	Equality (==, /=)	5 == 5
-- Ord	Ordering (<, >, compare)	"cat" > "dog"
-- Show	Convert to String (show)	show True → "True"
-- Read	Convert from String (read)	read "5" :: Int → 5
-- Enum	Sequential types (succ, pred, [x..y])	succ 'a' → 'b'
-- Num	Numeric operations (+, -, *)	3 + 5 → 8




--------------------------------------------
-- Enum Typeclass
---------------------------------------------

-- >>> enumFromTo 'c' 'f'
-- "cdef"

-- >>> :t enumFromTo 
-- enumFromTo :: Enum a => a -> a -> [a]

-- An Enum type in Haskell refers to types that have a sequential ordering. These types belong to the Enum typeclass, which provides functions for enumeration (listing values in order).



-- >>> succ 3   
-- 4

-- >>> succ 'a' 
 
--- >>> succ False  
-- 'b'
-- True

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Enum, Show)

-- >>> succ Mon   
-- Tue

-- >>> succ Sat    
-- Sun

--- >>> succ Sun
-- succ{Day}: tried to take `succ' of last tag in enumeration


-- >>>  succ Mon
-- Tue




--------------------------------------------
-- Ord
---------------------------------------------
-- The Ord typeclass in Haskell is used for ordering (comparison) between values. Any type that is an instance of Ord supports comparison operations like:

-- (<) (less than)
-- (>) (greater than)
-- (<=) (less than or equal to)
-- (>=) (greater than or equal to)
-- compare (returns ordering: LT, EQ, or GT)







--------------------------------------------
-- Length 
---------------------------------------------


-- ghci> :t length
-- length :: Foldable t => t a -> Int

-- ghci> length [(+) , (-) , \x y -> x *x + y]
-- 3

-- all three are the function , hence , same type. 








-- >>> take
-- No instance for (Show
--                    (Int -> [a0_a2rX3[tau:0]] -> [a0_a2rX3[tau:0]]))
--   arising from a use of `evalPrint'
--   (maybe you haven't applied a function to enough arguments?)
-- In a stmt of an interactive GHCi command: evalPrint it_a2rVC


-- What’s Happening?
-- GHCi tries to print whatever you enter.
-- take is a function, but you didn’t provide arguments.
-- Haskell doesn’t know how to print functions because Show (which converts values to strings) is not implemented for functions.


-- Functions cannot be printed using Show because they are not data like Int or String.

-- means that you haven't really made a mistake here you've just typed in an expression that can't be shown





-- ghci> :t print 
-- print :: Show a => a -> IO ()


-- The type () is a type containing just one value () .
-- • The type IO () denotes an 10 action yielding no interesting result, but having the side effect of printing the argument to the screen.
-- • The argument is flexible, but constrained to be an instance of the Show class.
-- • Functions are not an instance of the Show class, hence the GHCi error when typing in anything of a functional type.
-- • In Haskell, all side-effecting operations are explicitly marked by being elements of the IO type.


-- ghci> print "hello , Haskell"
-- "hello , Haskell"

-- ghci> print 43
-- 43

-- ghci> print (Just 34)
-- Just 34


-- print takes any value of type a, as long as a has an instance of the Show typeclass.

-- It converts the value to a string using show and then outputs it using putStrLn.




---------------------------------------------
-- Difference Between print and putStrLn
---------------------------------------------

-- putStrLn works only with String values.
-- print works with any type that implements Show by converting it to a string first.


-- ghci> :i Int
-- type Int :: *
-- data Int = GHC.Types.I# GHC.Prim.Int#
--         -- Defined in ‘GHC.Types’
-- instance Bounded Int -- Defined in ‘GHC.Enum’
-- instance Enum Int -- Defined in ‘GHC.Enum’
-- instance Integral Int -- Defined in ‘GHC.Real’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Real Int -- Defined in ‘GHC.Real’
-- instance Read Int -- Defined in ‘GHC.Read’
-- instance Eq Int -- Defined in ‘GHC.Classes’
-- instance Ord Int -- Defined in ‘GHC.Classes’
-- instance Show Int -- Defined in ‘GHC.Show’




-- ghci> putStrLn (show 34)
-- 34




-- what is explicit Effect 

-- What Does "Explicit Effect" Mean?
-- In Haskell, effects are not hidden inside functions like in imperative languages. Instead, functions that perform effects must indicate it in their type signatures. This makes it clear which functions are pure and which involve side effects.


-- pureFunction :: Int -> Int
-- pureFunction x = x * 2  -- No side effects

-- impureFunction :: IO ()
-- impureFunction = putStrLn "Hello, World!"  -- Side effect: printing to the console


-- main :: IO ()
-- main = do
--     putStrLn "Enter a number:"
--     input <- getLine
--     let num = read input :: Int
--     let result = Just num >>= (\x -> return (x + 2)) -- Using `>>=` in IO
--     print result



