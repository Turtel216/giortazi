-- | Utility functions for filling gaps between the modules
module Utils where

import Data.Time
import GHC.Num
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)

-- | Calculate Greek Orthodox Easter Sunday for a given year
orthodoxEaster :: Integer -> Day
orthodoxEaster year =
  let -- Calculate the precise Julian-Gregorian offset
      centuryDiff = year `div` 100
      leapAdjust = centuryDiff `div` 4
      julianOffset = centuryDiff - leapAdjust - 2

      -- Julian Easter calculation (Gauss algorithm adapted for Julian)
      a = year `mod` 4
      b = year `mod` 7
      c = year `mod` 19
      d = (19 * c + 15) `mod` 30
      e = (2 * a + 4 * b - d + 34) `mod` 7
      month = (d + e + 114) `div` 31
      day = ((d + e + 114) `mod` 31) + 1

      -- Create Julian Easter date and convert to Gregorian
      julianEaster = fromGregorian year (integerToInt month) (integerToInt day)

  in addDays julianOffset julianEaster

-- | Get the current year
getCurrentYear :: IO Integer
getCurrentYear = do
    now <- getZonedTime
    let (year, _, _) = toGregorian (localDay (zonedTimeToLocalTime now))
    return year

-- | Convert a date string from "YYYY-MM-DD" to "DD/MM"
convertDate :: String -> String
convertDate dateStr =
  let parts = splitOn "-" dateStr
  in case parts of
      [_ , month, day] -> day ++ "/" ++ month
      _ -> error "Invalid date format, expected YYYY-MM-DD" -- TODO: Handle this more gracefully

-- | Split a list on a given separator
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn sep xs =
  case break (== head sep) xs of
    (chunk, []) -> [chunk]
    (chunk, _:rest) -> chunk : splitOn sep rest

-- | Function to run two functions concurrently and wait for both to finish
concurrently :: IO a -> IO b -> IO (a, b)
concurrently action1 action2 = do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    _ <- forkIO $ action1 >>= putMVar mvar1
    _ <- forkIO $ action2 >>= putMVar mvar2

    result1 <- takeMVar mvar1
    result2 <- takeMVar mvar2

    return (result1, result2)

-- | Convert the number of days from Easter to the corresponding day of the year
fromToEasterToDay :: Int -> Integer -> String
fromToEasterToDay toEaster year =
  let easterDate = orthodoxEaster (fromIntegral year)
      targetDate = addDays (fromIntegral toEaster) easterDate
  in convertDate $ show targetDate
