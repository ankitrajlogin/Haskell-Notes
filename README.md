# HASKELL-Notes

# data vs newtype vs type in Haskell

Haskell provides three ways to define new types: `data`, `newtype`, and `type`. Each has different use cases and trade-offs.

## 1️⃣ data: Defining a Completely New Type

The `data` keyword is used to create new algebraic data types (ADTs) with one or more constructors.

### Syntax

```haskell
data Shape = Circle Float | Rectangle Float Float
    deriving Show
```

### Key Features

✅ Can have multiple constructors  
✅ Can store different types of values  
✅ Fully independent type  

### Example Usage

```haskell
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main = print (area (Circle 5))  -- Output: 78.54
```

### When to Use `data`?
- When you need multiple constructors (e.g., `Circle | Rectangle`).
- When defining complex data structures (e.g., trees, linked lists).
- When the type needs to be completely independent from existing types.

---

## 2️⃣ newtype: Defining a Lightweight Wrapper

The `newtype` keyword is used when you need a new type that wraps an existing type with only one constructor.

### Syntax

```haskell
newtype Age = Age Int
    deriving (Show, Eq, Ord)
```

### Key Features

✅ Performance-optimized: GHC treats `newtype` as a wrapper at compile time, so there's no runtime overhead.  
✅ Can be used to create distinct types (e.g., `Age` vs. `Int`).  
❌ Only one constructor allowed (unlike `data`).  

### Example Usage

```haskell
getAge :: Age -> Int
getAge (Age n) = n

main = print (getAge (Age 30))  -- Output: 30
```

### When to Use `newtype`?
- When you need a type distinction but no extra runtime cost.
- When you only need one constructor.
- When working with type class instances (e.g., deriving `Eq`, `Ord` for new types).

---

## 3️⃣ type: Creating Type Aliases

The `type` keyword creates a type alias, meaning it does not introduce a new type—just a different name for an existing type.

### Syntax

```haskell
type Name = String
type Age = Int
```

### Key Features

✅ No runtime overhead (it’s just an alias).  
✅ Makes code more readable.  
❌ Does not create a new type (no type safety improvements).  
❌ No constructors (unlike `data` or `newtype`).  

### Example Usage

```haskell
type Person = (Name, Age)

getName :: Person -> Name
getName (n, _) = n

main = print (getName ("Alice", 25))  -- Output: "Alice"
```

### When to Use `type`?
- When you just need a shorthand for an existing type.
- When defining complex types to improve readability (e.g., `type Person = (String, Int)`).
- When performance is critical (no extra wrapping like `newtype`).

---

## 4️⃣ Key Differences: `data` vs `newtype` vs `type`

| Feature              | `data` | `newtype` | `type` |
|----------------------|--------|-----------|--------|
| Creates a new type?  | ✅ Yes | ✅ Yes    | ❌ No (just an alias) |
| Multiple constructors? | ✅ Yes | ❌ No | ❌ No |
| Runtime overhead?    | 🔴 Yes (extra wrapping) | 🟢 No (optimized by GHC) | 🟢 No (just an alias) |
| Can store multiple values? | ✅ Yes | ✅ Yes (one constructor only) | ❌ No (alias only) |
| Used for pattern matching? | ✅ Yes | ✅ Yes | ❌ No |

---

## 5️⃣ Which One Should You Use?

| Situation | Use |
|-----------|-----|
| Need a completely new type with multiple constructors | `data` |
| Need a distinct type but with only one constructor (for optimization) | `newtype` |
| Just need an alias for an existing type (no type safety) | `type` |

---

## 6️⃣ Example Showing All Three

```haskell
-- Using `data` (Multiple constructors)
data Shape = Circle Float | Rectangle Float Float
    deriving Show

-- Using `newtype` (Single constructor, optimized)
newtype Age = Age Int
    deriving Show

-- Using `type` (Alias, no new type)
type Name = String

-- Function using `newtype`
showAge :: Age -> String
showAge (Age n) = "Age: " ++ show n

-- Function using `type`
getName :: Name -> String
getName n = "Name: " ++ n

main = do
    print (Circle 10)     -- Output: Circle 10.0
    print (showAge (Age 25))  -- Output: "Age: 25"
    print (getName "Alice")   -- Output: "Name: Alice"
```

---

## Conclusion

- Use `data` when creating completely new types with multiple constructors.
- Use `newtype` when wrapping a single value for type safety and performance.
- Use `type` when you just need a shorthand alias.

🚀 Hope that clarifies everything! Let me know if you need more examples. 😃
