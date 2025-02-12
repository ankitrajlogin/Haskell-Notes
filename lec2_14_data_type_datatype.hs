
-- Introduces the new datatypes Type and the data constructor Con1 , con2 , ....

{-# OPTIONS_GHC -Wall #-}
module Functions where



-- a node can be leaf node that has value or can have left subtree or right subtree. 
-- intermediate note have no value. 

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)

-- ghci> :t Node
-- Node :: Tree a -> Tree a -> Tree a

exampleTree :: Tree Int
exampleTree =
    Node (Leaf 2) (Leaf 3)

exampleTree2 :: Tree Int
exampleTree2 = Node (Node (Leaf 4) (Leaf 5)) (Leaf 6) 

data Choice = Rock | Paper | Scissors 


data Weekday = Mo | Tu | We | Th | Fr | Sa | Su


data User' = User' Int String String 
    deriving Show

-- ghci> User 23 "Ankit" "Raj"
-- User 23 "Ankit" "Raj"

-- (Int , String , String) 

data User = User {uid :: Int , uname :: String , uemail :: String}
    deriving Show 

-- ghci> mohan = User {uid = 43 , uname = "Ankit",  uemail = "ankit@gmail.com"}
-- ghci> uid mohan 
-- 43
-- ghci> uname mohan 
-- "Ankit"


-- Note : we can't use mohan.uname in haskell as default. 


-- there are many different kinds of strings and not all of them are supposed to supoort the same operation. So , in general it makes sense to add to integers and to compute their sum, but for user IDs. it probably does not make sense. 

data UserId = UserId Int

uid1 :: UserId 
uid1 = UserId 42

uid2 :: UserId
uid2 = UserId 324


data Email = Email String 
    deriving Show 

email23 :: Email 
email23 = Email "Ankit@gmail.com"


-- Hence we can define user as using this. 

data User'' = User'' {uid_ :: UserId , uname_ :: String , uemail_ :: Email }



-- newtype is a way to define a new type that is distinct from an existing type but has the same underlying representation at runtime. It is often used for creating type-safe abstractions while maintaining minimal runtime overhead.

newtype UserID = UserID Int
    deriving (Show, Eq)

newtype EmailID = EmailID String
    deriving (Show, Eq)








type InList = [Int]
 
x :: InList 
x = [234,234,234,234]


type Testerval  = Int 

---- Final Call --------


--------------------------------

-- data tester_val = Int


-- Issues in Your Code
-- The problem here is that Int is already a built-in type in Haskell.
-- This definition makes Int a constructor, which conflicts with Haskell’s built-in Int type.
-- Also, the tester_val type does not allow storing actual integer values



-- Correcting the Code
-- If you want a custom data type that can store an integer value, you should do:

data TesterVal = TesterInt Int  -- Constructor that wraps an Int
    deriving Show


-- Best approach :

newtype TesterValue = TesterIntval Int
    deriving Show

-- This is a valid approach, and in this case, it's actually preferable to data because:

-- newtype introduces zero runtime overhead (GHC treats TesterVal just like Int under the hood).
-- You only have one constructor (TesterInt), making newtype a great fit.

