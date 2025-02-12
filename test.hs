import qualified Data.Time as Time
import qualified Data.Time.Format as Format
import qualified Data.Text as DataText

-- -- Function to convert UTC time to IST
-- utcToIST :: Time.UTCTime -> Time.LocalTime
-- utcToIST utcTime =
--   let istZone = Time.minutesToTimeZone (5 * 60 + 30)  -- IST is UTC+5:30
--   in Time.utcToLocalTime istZone utcTime

-- -- Function to parse UTC time from a string
-- parseUTC :: String -> Maybe Time.UTCTime
-- parseUTC = Format.parseTimeM True Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- main :: IO ()
-- main = do
--   let utcString = "2020-12-08T07:29:28Z"  -- Hardcoded input
--   case parseUTC utcString of
--     Just utcTime -> do
--       let istTime = utcToIST utcTime
--       putStrLn $ "UTC Time: " ++ show utcTime
--       putStrLn $ "IST Time: " ++ show istTime
--     Nothing -> putStrLn "Invalid input format."



dateCreated :: String
dateCreated = "2020-12-08T07:29:28Z"

formattedDate2 :: String -> Maybe Time.UTCTime
formattedDate2 = Format.parseTimeM True Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"




newformattedDate = Time.formatTime Time.defaultTimeLocale "%d%%2F%m%%2F%Y+%H%%3A%M%%3A%S" formattedDate2

date = DataText.pack $ newformattedDate