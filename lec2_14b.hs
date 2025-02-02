

-- 1. data
-- The data keyword is used to define algebraic data types in Haskell. This is the most versatile way to create new types, as it allows for multiple constructors, fields, and recursion.

-- Features:
-- Can have multiple constructors.
-- Each constructor can have zero or more fields.
-- Used for defining complex types like enums, records, or recursive structures.


data Color = Red | Green | Blue
    deriving Show

-- ghci> Red
-- Red

data Person = Person { name :: String, age :: Int }
    deriving Show

-- ghci> let p = Person { name = "Alice", age = 30 }
-- ghci> name p
-- "Alice"





-- 2. newtype
-- The newtype keyword is used to create a new type that is distinct from its underlying type but has the same runtime representation. It is more efficient than data because it avoids runtime overhead.

-- Features:
-- Must have exactly one constructor with exactly one field.
-- More efficient than data because there is no extra runtime wrapping.
-- Commonly used for type safety.


newtype UserID = UserID Int
    deriving Show

-- UserID is a new type that wraps an Int.

-- ghci> let uid = UserID 123
-- ghci> uid
-- UserID 123
-- ghci> :t uid
-- uid :: UserID

-- Key Point: Even though UserID is represented as an Int internally, it is a distinct type. You can't accidentally mix it with plain Int values.


-- 3. type
-- The type keyword creates a type synonym, which is just another name for an existing type. It doesn't create a new type; it only provides an alias for readability and convenience.

-- Features:
-- Does not introduce a new type.
-- No runtime overhead (just an alias).
-- Useful for simplifying complex type signatures.


type UserName = String
type Age = Int


-- ghci> let name = "Alice" :: UserName
-- ghci> let age = 30 :: Age

