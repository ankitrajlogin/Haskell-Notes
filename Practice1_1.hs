
main = do 
    let var1 = 2
    let var2 = 3
    print (var1 + var2) 




chat :: IO()
chat = do
    print [1..10]

--  ghci> chat
-- [1,2,3,4,5,6,7,8,9,10]

fact :: Int -> Int
fact 0 = 1
fact n = n* fact (n-1)

factorial :: IO ()
factorial = do
    print(fact 3)



fact' :: Int -> Int
fact' n 
    | n == 0 = 1
    | n /= 0 = n * fact (n-1)


roots :: (Float , Float , Float) -> (Float , Float)
roots (a,b,c) = (x1 , x2) where 
    x1 = e + sqrt d /(2*a)
    x2 = e - sqrt d / (2*a)
    d = b*b - 4*b*c
    e = -(b / (2*a))

rootVal :: IO ()
rootVal = do 
    print(roots(1,-8,6))

-- ghci> rootVal
-- (12.0,-4.0)



eveno :: Int -> Bool
noto :: Bool -> String

eveno x = if x `rem` 2 == 0 then True else False

noto x = if x == True then "This is an even Number" else "This is an ODD Number" 

checkEven = do 
    print ((noto.eveno)16)






sumAndSubtract :: Int -> Int -> (Int, Int)
sumAndSubtract a b = (a + b, a - b)

-- Example usage:
mainSS :: IO ()
mainSS = do
    let (sumResult, subResult) = sumAndSubtract 10 5
    print ("Sum:", sumResult)
    print ("Subtraction:", subResult)


-- You can write the type signature of a function that takes two integers and returns two integers (one for sum and one for subtraction) using a tuple.

-- Int -> Int -> (Int, Int)
-- The function takes two Int values as input.
-- It returns a tuple (Int, Int), where:
-- The first element of the tuple is the sum (a + b).
-- The second element is the subtraction (a - b).


