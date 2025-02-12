
class Convertible a where
    convertToString :: a -> String


-- Instance for Int
instance Convertible Int where
    convertToString x = show x

-- Instance for Double
instance Convertible Double where
    convertToString x = show x

-- Instance for Bool
instance Convertible Bool where
    convertToString True = "Yes"
    convertToString False = "No"

instance Convertible String where
    convertToString a = a 


-- >>> convertToString (42 :: Int) 
-- "42"


-- >>> convertToString (42 :: Double) 
-- "42.0"


-- >>> convertToString (23.2 :: Double)
-- "23.2"

-- >>> convertToString (True)
-- "Yes"


-- >>> convertToString ("Ankit")
-- "Ankit"





-- Serialization and Deserialization in Haskell
-- We will create a Serializable class that defines methods for converting data to a string (serialization) and parsing a string back to data (deserialization).

-- Define the Serializable type class
class Serializable a where
    -- Converts the value into a string (serialization)
    toString :: a -> String
    
    -- Converts the string back into a value (deserialization)
    fromString :: String -> Maybe a

-- Instance for Int
instance Serializable Int where
    toString = show  -- Convert Int to String using show
    fromString str = Just (read str :: Int)  -- Convert String back to Int using read

-- Instance for Bool
instance Serializable Bool where
    toString True = "True"
    toString False = "False"
    fromString "True" = Just True
    fromString "False" = Just False
    fromString _ = Nothing

-- Instance for String (essentially identity)
instance Serializable String where
    toString = id  -- String is already a string, so no conversion
    fromString = Just  -- Anything is valid as a String, so return it as a Just value

-- Custom type: Person
data Person = Person { name :: String, age :: Int } 
    deriving Show

-- Instance for Person
instance Serializable Person where
    toString (Person name age) = name ++ "," ++ show age
    fromString str = 
        let parts = wordsWhen (== ',') str
        in if length parts == 2
            then Just (Person (head parts) (read (last parts) :: Int))
            else Nothing

-- Utility function to split strings (helper function)
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

-- Testing with various types
main3 :: IO ()
main3 = do
    -- Test serialization and deserialization for different types
    let serializedInt = toString (42 :: Int)
    let deserializedInt = fromString serializedInt :: Maybe Int

    let serializedBool = toString True
    let deserializedBool = fromString serializedBool :: Maybe Bool

    let serializedPerson = toString (Person "Alice" 30)
    let deserializedPerson = fromString serializedPerson :: Maybe Person

    -- Print results
    print ("Serialized Int: " ++ serializedInt)
    print ("Deserialized Int: " ++ show deserializedInt)

    print ("Serialized Bool: " ++ serializedBool)
    print ("Deserialized Bool: " ++ show deserializedBool)

    print ("Serialized Person: " ++ serializedPerson)
    print ("Deserialized Person: " ++ show deserializedPerson)

