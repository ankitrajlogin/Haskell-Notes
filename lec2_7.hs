

-- ghci> [1..50]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]
-- ghci> map (\x -> x +1) [1..50]
-- [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51]
-- ghci> map (\x -> x*x) [1..50]
-- [1,4,9,16,25,36,49,64,81,100,121,144,169,196,225,256,289,324,361,400,441,484,529,576,625,676,729,784,841,900,961,1024,1089,1156,1225,1296,1369,1444,1521,1600,1681,1764,1849,1936,2025,2116,2209,2304,2401,2500]
-- ghci> map (min 2) [1..50]
-- [1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]
-- ghci> map (2-) [1..5]
-- [1,0,-1,-2,-3]
-- ghci> map (-2) [1..5]






------------------------------------
-- Find an invocation of filter that finds all non-empty lists in a list of lists.
------------------------------------

filter_list :: [[a]] -> [[a]] 
filter_list [] = [] 
filter_list (x : xs) = if null x then  filter_list xs else x : filter_list xs 

-- >>> filter_list [[] , [2134,234,234] , []]
-- [[2134,234,234]]




--------------------
-- ghci> :t id
-- id :: a -> a

-- What does id do?
-- id is the identity function.
-- It returns whatever it is given unchanged.


-- Step 3: What Does filter id Do?
-- Since filter keeps elements where the function returns True, filter id simply removes False values and keeps only True values.



-------------------------------------
-- self test 
---------------------------------------
-- ghci> filter (/=0) [2,4,6,0,8,0,0,9,8]
-- [2,4,6,8,9,8]




-- ghci> :t map (3 `elem`)
-- map (3 `elem`) :: [[Int]] -> [Bool]


-- ghci> map (3 `elem`) [[1,2,3], [4,5,6], [3,7,8], []]
-- [True,False,True,False]

-- ghci> 2 : []
-- [2]




-- (: [])	a -> [a]	Wraps an element in a single-element list.
-- ([] :)	[[a]] -> [[a]]	Prepends [] to a list of lists.


-- >>> (: []) 23  -- correct way to write this.
-- [23]

-- >>> map (: []) [1,2,3]
-- [[1],[2],[3]]


-- >>> [] : [2,3,4]
-- No instance for (Num [()]) arising from a use of `it_a5D7h'
-- In the first argument of `evalPrint', namely `it_a5D7h'
-- In a stmt of an interactive GHCi command: evalPrint it_a5D7h

-- >>> [] : [[1,2]]
-- [[],[1,2]]


