module Into where

five = 434

six = 34*345

alist = [345,3453,324,six , five] 

doubled x = x*x 

sumTwo a b = a+b 

-- function binding 

double :: Integer -> Integer
double = \x -> x+x  -- better to use direct. 

dou x = x+x 

distance x y = abs(x-y) 

dist = \x y -> abs(x-y)  -- correct code but better to use above



-- type in the datatypes choice 
-- data is used to define new data types in haskell
data Choice = Rock | Paper | Scissors
    deriving Show


-- Haskell requries constructor name to start with uppercase letter
-- deriving show automatically makes the choice type an instance of the show typeclass
-- this means value of type choice can be converted to strings for display purpose. 

data Newfind = Ankit | Rahul | Sayam 
    deriving Show



-- improve: The name of the function.
-- Choice -> Choice: This means improve is a function that takes an input of type Choice and returns an output of the same type (Choice).
improve :: Choice -> Choice
improve Rock = Paper
improve Paper = Scissors
improve Scissors = Rock


