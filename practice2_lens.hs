
import Prelude ; 

-- Explain authzResponse ^. _mandate (Lens Usage)
-- Let's break this down with an easy-to-understand example.

-- Step 1: Define a Data Structure
-- We define a simple AuthzResponse data type that contains a _mandate field.




-- Define a simple data structure
data AuthzResponse = AuthzResponse
  { _status  :: String  -- Status of the authorization (e.g., "Approved", "Declined")
  , _mandate :: Maybe String  -- Mandate ID (optional)
  } deriving (Show)

-- Automatically generate lenses for the fields
-- makeLenses ''AuthzResponse
-- Step 2: Create an Example Object
-- We create an instance of AuthzResponse.

response :: AuthzResponse
response = AuthzResponse "Approved" (Just "M12345")


-- This means we have:
-- AuthzResponse { _status = "Approved", _mandate = Just "M12345" }



-- Step 3: Extract _mandate Using Lens (^.)
-- Now, let's extract the _mandate field using the lens operator ^.:

main :: IO ()
main = do
  let mandateValue = response ^. _mandate  -- Extracts `_mandate` field
  print mandateValue


-- Output:
-- Just "M12345"


-- Explanation
-- response ^. _mandate extracts the _mandate field from the response object.
-- Since _mandate is of type Maybe String, the result is Just "M12345".
-- If _mandate was Nothing, it would return Nothing.
-- Bonus: Updating _mandate Using Lenses (.~)
-- You can also modify _mandate using .~:



-- haskell
-- Copy
-- Edit
-- updatedResponse = response & _mandate .~ Just "M67890"


-- This creates a new record with _mandate = Just "M67890", keeping the rest unchanged.

-- Printing updatedResponse gives:
-- AuthzResponse { _status = "Approved", _mandate = Just "M67890" }




-- Key Takeaways
-- ^. extracts a field from a record.
-- & ... .~ ... updates a field immutably.
-- Lenses make it easy to work with nested data in a functional way.
