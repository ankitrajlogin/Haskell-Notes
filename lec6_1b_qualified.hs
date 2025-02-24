


--------------------------------------------------------------------
-- Use of qualified in Haskell (import qualified Data.Map.Strict as M)
--------------------------------------------------------------------

-- In Haskell, the qualified keyword is used during imports to avoid name conflicts and make it explicit where functions come from. Let's break it down with an example.

--------------------------------------------------------------------
-- 1. With qualified Import
--------------------------------------------------------------------

import qualified Data.Map.Strict as M

main2 = do
    let myMap = M.fromList [(1, "one"), (2, "two")]
    print $ M.lookup 1 myMap



-- Data.Map.Strict provides a function fromList, but since it's imported qualified, you must reference it as M.fromList.
-- This makes it clear that fromList comes from Data.Map.Strict, avoiding potential conflicts with other modules that may define fromList.



--------------------------------------------------------------------
-- 2. Without qualified Import
--------------------------------------------------------------------

-- import Data.Map.Strict

-- main = do
--     let myMap = fromList [(1, "one"), (2, "two")]
--     print $ lookup 1 myMap


-- Here, fromList and lookup are imported without qualification, so you can use them directly.
-- However, this might cause conflicts if another module (e.g., Data.Set) also provides a fromList function.






--------------------------------------------------------------------
-- What Happens If You Don't Use qualified?
--------------------------------------------------------------------
-- Potential Name Conflicts:
-- If another module defines fromList, Haskell won't know which one to use.


-- Less Readability:
-- It may not be obvious where a function comes from, especially in large projects.


-- Implicit Imports:
-- Without qualification, all functions from the module become available in the global scope, which might lead to accidental usage of unintended functions.'




--------------------------------------------------------------------
-- When to Use qualified
--------------------------------------------------------------------

-- When importing large modules with many functions (Data.Map, Data.Set, etc.).
-- When similar function names exist in different modules.
-- When you want to explicitly indicate which module a function belongs to for better readability.
