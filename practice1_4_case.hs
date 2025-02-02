

-- case Expression in Haskell
-- In Haskell, case expressions allow pattern matching on values. They are useful for handling multiple possible values in a clean and expressive way.





-------------------------------
-- 1. Basic Syntax
-------------------------------

-- A case expression has the following form:


-- case expression of
--     pattern1 -> result1
--     pattern2 -> result2
--     ...
--     _        -> defaultResult  -- (optional) catch-all pattern


-- expression is evaluated.
-- If expression matches pattern1, it evaluates to result1, and so on.
-- _ is a wildcard that matches any value not covered by previous patterns.




-------------------------------ß
-- 2. Pattern Matching on Numbers 
-------------------------------

describeNumber :: Int -> String 
describeNumber x = 
    case x of 
        0 -> "Zero" 
        1 -> "One"
        _ -> "Something else" 


-- >>> describeNumber 1 
-- "One"

-- >>> describeNumber 43
-- "Something else"




-------------------------------
-- 3. Pattern Matching on Lists 
-------------------------------

listDescription :: [Int] -> String 
listDescription xs = 
    case xs of 
        [] -> "Empty list" 
        [x] -> "Single element list" 
        [x , y] -> "Two elements list" 
        _ -> "A longer list"


-- >>> listDescription [2,3]
-- "Two elements list"

-- >>> listDescription [2,3,4]
-- "A longer list"





-------------------------------
-- Solve these problem using case 
-------------------------------

-- 1. classify a number that is postive or negative. 


classifyNumber :: Int -> String 
classifyNumber n = 
    case compare n 0 of  -- compare n 0 give 3 value LT , EQ and GT 
        LT -> "Negative"
        EQ -> "Zero"
        GT -> "Positive"


-- >>> classifyNumber 32
-- "Positive"




-- 2. Write a function that returns the Fibonacci number at position n.

fibonacci :: Int -> Int
fibonacci n = case n of
    0 -> 0
    1 -> 1
    _ -> fibonacci (n - 1) + fibonacci (n - 2)





-- 3. Given three side lengths, classify the triangle as Equilateral, Isosceles, or Scalene.

triangleType :: Int -> Int -> Int -> String
triangleType a b c = case (a == b, b == c, a == c) of
    (True, True, True)   -> "Equilateral"
    (True, False, False) -> "Isosceles"
    (False, True, False) -> "Isosceles"
    (False, False, True) -> "Isosceles"
    _                    -> "Scalene"












-------------------------------
-- Complex Problem 
-------------------------------

-- Define a data type for arithmetic expressions
data Expr
    = Val Int            -- A single integer value
    | Add Expr Expr      -- Addition of two expressions
    | Sub Expr Expr      -- Subtraction
    | Mul Expr Expr      -- Multiplication
    | Div Expr Expr      -- Division
    deriving (Show)

-- Recursive function to evaluate the expression
eval :: Expr -> Int
eval expr = case expr of
    Val n     -> n
    Add e1 e2 -> eval e1 + eval e2
    Sub e1 e2 -> eval e1 - eval e2
    Mul e1 e2 -> eval e1 * eval e2
    Div e1 e2 -> case eval e2 of
        0 -> error "Division by zero!"  -- Handle division by zero
        n -> eval e1 `div` n


expr1 = Add (Val 3) (Mul (Val 4) (Val 2))  -- (3 + (4 * 2))
expr2 = Sub (Mul (Val 6) (Val 3)) (Div (Val 8) (Val 2))  -- ((6 * 3) - (8 / 2))
expr3 = Div (Val 10) (Val 0)  -- Should trigger division by zero error


-- >>> eval expr1 
-- 11

-- >>> eval expr3 
-- Division by zero!

-- since division by zero is not allowed, the Div expression will trigger the error handling in the eval function.






