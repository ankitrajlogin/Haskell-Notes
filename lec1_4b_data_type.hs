



data Choice = Rock | Paper | Scissors
    deriving (Show , Eq)


-- 1. Understanding data Choice
-- The data keyword is used to define a new algebraic data type (ADT). Here, Choice is a type that can have one of three values:
-- Rock
-- Paper
-- Scissors

-- Each of these is called a constructor, and they act like distinct values belonging to the Choice type.



-- What Does deriving Show Do?
-- The deriving Show part automatically makes Choice an instance of the Show type class. This means Haskell will automatically define how Choice values should be converted to Strings.


-- Example: Printing Values
-- Since Choice derives Show, you can now print values of type Choice:

main :: IO ()
main = do
    print Rock       -- Output: Rock
    print Paper      -- Output: Paper
    print Scissors   -- Output: Scissors


-- ghci> main 
-- Rock
-- Paper
-- Scissors

describe :: Choice -> String
describe Rock     = "Rock is strong!"
describe Paper    = "Paper covers rock."
describe Scissors = "Scissors cut paper."


main2 :: IO ()
main2 = do
    putStrLn (describe Rock)      -- Output: Rock is strong!
    putStrLn (describe Paper)     -- Output: Paper covers rock.
    putStrLn (describe Scissors)  -- Output: Scissors cut paper.




-- 5. Extending with More Type Classes

-- Eq: Allows comparisons like Rock == Rock (returns True).
-- Ord: Enables ordering, meaning Rock < Paper can be evalu



-- Haskell's Ord type class requires a total order, meaning the relation must be transitive and antisymmetric.




-- Convince yourself that for all elements x of type Choice, calling worsen (improve x) yields x again.


worsen ::  [Choice] -> [Choice]
worsen [] = [] 
worsen (x : xs) = 
    if x == Rock then Scissors : worsen xs 
    else if x == Paper then Rock : worsen xs 
    else  Paper : worsen xs 


-- ghci> worsen [Rock, Paper , Scissors , Paper , Rock]
-- [Scissors,Rock,Paper,Rock,Scissors]



worsen2 :: [Choice] ->[Choice]
worsen2 [] = [] 
worsen2 (x :xs) 
    | x == Rock = Scissors : worsen2 xs 
    | x == Paper = Rock : worsen2 xs 
    | otherwise = Paper : worsen2 xs 



-- Solve using Let 

worsen3 :: [Choice] -> [Choice]
worsen3 [] = [] 
worsen3 (x : xs) = 
    let 
        newChoice = if x == Rock then Scissors
                    else if x == Paper then Rock
                    else Paper
    in 
        newChoice : worsen2 xs 



-- using >>= 
-- Define a function to worsen a single choice
worsenChoice :: Choice -> [Choice]
worsenChoice Rock     = [Paper]
worsenChoice Paper    = [Scissors]
worsenChoice Scissors = [Rock]



worsen4 :: [Choice] -> [Choice]
worsen4 xs = xs >>= worsenChoice  -- Applies worsenChoice to each element

-- ghci> worsen4 [Rock, Paper , Scissors , Paper , Rock]
-- [Paper,Scissors,Rock,Scissors,Paper]



-----------------------------
-- How >>= Works in Lists
--------------------------------

-- For lists, >>= (bind) works like map but also flattens the result.

-- Equivalent step-by-step transformation:

-- worsen4 [Rock, Paper, Scissors]
-- Step 1: Apply `worsenChoice` to each element
-- Rock -> [Scissors]
-- Paper -> [Rock]
-- Scissors -> [Paper]

-- Step 2: Flatten the result
-- [[Scissors], [Rock], [Paper]]  -- Nested lists
-- => [Scissors, Rock, Paper]      -- Flattened list




-------------------------------------------------
-------------------------------------------------


-- (>>=) :: [a] -> (a -> [b]) -> [b]



-- >>> [1,2,3,4,5] >>= (\x -> [x + 5])
-- [6,7,8,9,10]


-- ghci> [1,2,3,4,5] >>= (\x -> [x + 5])
-- [6,7,8,9,10]



