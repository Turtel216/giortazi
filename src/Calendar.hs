{-# LANGUAGE  RecordWildCards #-}
module Calendar where

import qualified Data.Text as T

-- | Data type representing a single day in the calendar
data CalendarDay = CalendarDay
  {
  day :: !T.Text,
  month :: !Month,
  currentYear :: !Int,
  names :: ![T.Text]
  } deriving(Eq, Show)

-- | Month in a year
data Month = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December deriving(Eq, Show)

-- | Converts a day in the calendar to a string that can be drawn by brick
dayToString :: CalendarDay -> String
dayToString CalendarDay {..} = T.unpack $ T.append (appendNL day) (T.unwords names)
  where
   appendNL = T.cons '\n'
