-- Understanding let ... in in Haskell
-- In Haskell, let ... in is used for defining local bindings (variables or functions) inside an expression. These bindings are only accessible within the scope of the in block.

-- Syntax

    -- let 
    --     <bindings> 
    -- in 
    --     <expression>


-- <bindings>: One or more local definitions.

-- <expression>: The result that uses these bindings.


-- Example : 

squareSum :: Int -> Int -> Int 
squareSum x y = 
    let 
        a = x*x 
        b = y*y 
    in 
        a +b 

main = print (squareSum 3 4) -- Output : 25 


-- Using let in do Blocks (IO Actions)
-- In do blocks, let is used without in:

mainS :: IO () 
mainS = do 
    let greeting = "Hello, Ankit" 
    putStrLn greeting 

-- Why no in?
-- In do blocks, let bindings are automatically available in the rest of the block.

-- Both let ... in and where create local bindings, but:

-- Feature	let ... in	                       where
-- Scope	Local to expression	               Local to function
-- Order	Comes before the expression	       Comes after the function body
-- Usable in GHCi?	✅ Yes	                   ❌ No



-- Example of where
-- squareSumWhere x y = a + b
--   where
--     a = x * x
--     b = y * y



-- INPUT 

-- evaluatePolynomial [3, 2, 5, 7] 2


-- OUTPUT , computation ( Using Horner's Rule) 

-- P(2) = (((3 * 2 + 2) * 2 + 5) * 2 + 7)
--      = ((6 + 2) * 2 + 5) * 2 + 7
--      = (8 * 2 + 5) * 2 + 7
--      = (16 + 5) * 2 + 7
--      = 21 * 2 + 7
--      = 42 + 7
--      = 49


evaluatePolynomial :: [Double] -> Double -> Double 
evaluatePolynomial coeffs x = 
    let 
        result = foldr (\c acc -> c + acc*x) 0 coeffs 
    in
        result 


mainE :: IO () 
mainE = do 
    let coeffs = [3,2,5,7]
    let x = 2 
    -- print $ evaluatePolynomial coeffs x 
    print (evaluatePolynomial coeffs x) 