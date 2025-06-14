module Utils where

import Data.Time
import GHC.Num
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)

-- Calculate Greek Orthodox Easter Sunday for a given year
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

getCurrentYear :: IO Integer
getCurrentYear = do
    now <- getZonedTime
    let (year, _, _) = toGregorian (localDay (zonedTimeToLocalTime now))
    return year

-- Function to run two functions concurrently and wait for both to finish
concurrently :: IO a -> IO b -> IO (a, b)
concurrently action1 action2 = do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    _ <- forkIO $ action1 >>= putMVar mvar1
    _ <- forkIO $ action2 >>= putMVar mvar2

    result1 <- takeMVar mvar1
    result2 <- takeMVar mvar2

    return (result1, result2)
