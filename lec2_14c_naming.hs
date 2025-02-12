-- Example: Type Definitions in Haskell
-- In Haskell, type names must start with an uppercase letter.
-- Otherwise, an error will occur because lowercase names are reserved for variables.

-- ❌ This will cause an error:
-- type testerval = Int  -- Error: Type names must start with uppercase letters!

-- ✅ This works because 'Testerval' starts with an uppercase letter:
type Testerval = Int  -- This is a valid type synonym.

-- General Haskell Naming Rules:
-- Haskell enforces a specific convention for naming various constructs.
-- Here are the general rules and examples:

-- 1️⃣ Type Synonyms (type):
-- Type names must start with an uppercase letter and follow CamelCase.
type Name = String  -- This is valid because 'Name' starts with an uppercase letter.

-- ❌ Invalid:
-- type person = String  -- Error! Must start with uppercase 'P'.

-- 2️⃣ Data Types (data):
-- Data type names must also start with an uppercase letter and use CamelCase.
data Person = Person String Int  -- Valid: 'Person' starts with an uppercase letter.

-- 3️⃣ Newtypes (newtype):
-- Similar to data types, newtypes must also follow the CamelCase convention.
newtype Age = Age Int  -- Valid: 'Age' starts with an uppercase letter.

-- 4️⃣ Type Variables (used in generics):
-- Type variables should start with a lowercase letter, typically one character long.
-- f :: a -> a  -- Valid: 'a' is a lowercase letter, as expected.

-- 5️⃣ Functions and Variables:
-- Function names and variable names must start with a lowercase letter and use camelCase.
-- sumNumbers :: [Int] -> Int  -- Valid: 'sumNumbers' follows camelCase.

-- 6️⃣ Constructor Names:
-- Constructor names for data and newtypes must start with an uppercase letter.
data Shape = Circle | Square  -- Valid: 'Circle' and 'Square' start with uppercase letters.

-- 7️⃣ Module Names:
-- Modules must start with an uppercase letter and use CamelCase.
-- module MyModule where  -- Valid: 'MyModule' starts with an uppercase letter.

-- 8️⃣ Operators:
-- Operators can be defined using symbols and do not follow the same case rules as functions.
-- (+) :: Int -> Int -> Int  -- Valid: This is a standard operator.

-- 9️⃣ Class Names:
-- Class names also follow the CamelCase convention and must start with an uppercase letter.
class Show a where  -- Valid: 'Show' starts with an uppercase letter.

-- 🔴 Record Field Names:
-- Record fields must start with a lowercase letter, often prefixed by the type name.
data User = User { userName :: String, userAge :: Int }  -- Valid: Fields start with lowercase.

-- ❌ Invalid record field:
-- data Person = Person { Name :: String, Age :: Int }  -- Error! Fields must start with lowercase.

-- 🔥 Constants:
-- Constants should not be in uppercase, use normal camelCase variable names instead.
piValue = 3.14  -- Valid: 'piValue' is lowercase and descriptive.

-- Summary of Naming Rules:
-- 1️⃣ Type names (type, data, newtype) - Uppercase + CamelCase
-- 2️⃣ Type variables - Lowercase (single letters like 'a', 'b')
-- 3️⃣ Functions and variables - Lowercase + camelCase
-- 4️⃣ Constructors - Uppercase + CamelCase
-- 5️⃣ Module names - Uppercase + CamelCase
-- 6️⃣ Operators - Can use symbols
-- 7️⃣ Class names - Uppercase + CamelCase
-- 8️⃣ Record fields - Lowercase (often prefixed with type name)
-- 9️⃣ Constants - Lowercase variables, no all-caps
