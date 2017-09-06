{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib
  ( Schedule(..)
  , Rule(..)
  , diffToNextMatch
  , nextDate
  , occurrences
  , nextMatch
  ) where

import Data.Dates
       (DateInterval(Days, Months, Weeks, Years), DateTime(..), WeekDay,
        addInterval, dateWeekDay, datesDifference, lastMonday,
        minusInterval, weekdayNumber)

import Data.Foldable (minimumBy)
import Data.Maybe
import Data.Time.Calendar (isLeapYear)
import Data.Time.Calendar.MonthDay (monthLength)

-- Apply a crude ordering to date intervals
-- We can't precisely compare (Days x) to (Months y)
-- without knowing the absolute dates we're talking about
-- (because different months have different numbers of days).
-- Instead of this, we order DateIntervals first by the size of
-- their constructor and then, within that, by their parameter.
-- Thus Years 2 > Years 1 > Months 13 > Months 1 > Weeks 6 > Weeks 1 > Days 10 > Days 1
-- Haskell's default derivation for Ord follows this pattern,
-- so we can just derive it here.
-- N.B. this will break if the order of constructors for DateInterval changes
deriving instance Ord DateInterval

data Schedule = Schedule
  { startTime :: DateTime
  , endTime :: DateTime
  , rules :: [Rule]
  } deriving (Show)

-- TODO: use enums instead of ints for these?
-- TODO: sort out the mess of Int vs Integer
data Rule
  = DayOfMonth Int
  | MonthOfYear Int
  | MonthlyInterval Int
  | WeeklyInterval Int
  | YearlyInterval Int
  | DayOfWeek WeekDay
  deriving (Show)

occurrences :: Schedule -> [DateTime]
occurrences Schedule {startTime, endTime, rules} =
  _occurrences rules startTime startTime endTime

-- TODO: could use whileJust here?
-- Because our date logic works by calculating the next valid date from the
-- current one, we start at one day before the start so that the start date
-- itself can be included as an occurrence.
_occurrences :: [Rule] -> DateTime -> DateTime -> DateTime -> [DateTime]
_occurrences rules current start end =
  drop 1 $ catMaybes $ iterate next (Just (prevDay current))
  where
    next :: Maybe DateTime -> Maybe DateTime
    next c =
      case c of
        Just d -> nextDate rules (nextDay d) start end
        Nothing -> Nothing

-- we find the next date with the following process:
-- check if the rules all match for the current date
-- if they do, return that
-- otherwise, find the smallest diffToNextMatch,
--  apply its corresponding validation to get the next candidate,
--  and recur
nextDate :: [Rule] -> DateTime -> DateTime -> DateTime -> Maybe DateTime
nextDate rules currentDate startDate endDate
  | currentDate > endDate = Nothing
  | all pairIsZero diffs = Just currentDate
  | otherwise = nextDate rules nextCandidate startDate endDate
  where
    pairIsZero :: (Rule, DateInterval) -> Bool
    pairIsZero = isZero . snd
    diffs = map (\v -> (v, diffToNextMatch v startDate currentDate)) rules
    minDiff = fst $ mapMinimum snd (filter (not . pairIsZero) diffs)
    nextCandidate = nextMatch minDiff startDate currentDate

nextMatch :: Rule -> DateTime -> DateTime -> DateTime
nextMatch v@(MonthlyInterval _) start current =
  DateTime (year next) (month next) 1 0 0 0
  where
    next = addInterval current $ diffToNextMatch v start current
nextMatch v@(WeeklyInterval _) start current = lastMonday next
  where
    next = addInterval current $ diffToNextMatch v start current
nextMatch v@(YearlyInterval _) start current = DateTime (year next) 1 1 0 0 0
  where
    next = addInterval current $ diffToNextMatch v start current
nextMatch v start current =
  addInterval current $ diffToNextMatch v start current

diffToNextMatch :: Rule -> DateTime -> DateTime -> DateInterval
diffToNextMatch (DayOfMonth dayOfMonth) _startDate currentDate =
  let diff = dayOfMonth - day currentDate
      untilNextMonth = daysInMonth currentDate + diff
  in if diff >= 0
       then Days (toInteger diff)
       else Days (toInteger untilNextMonth)
diffToNextMatch (MonthOfYear monthOfYear) _startDate currentDate =
  let currentMonth = month currentDate
      diff = monthOfYear - currentMonth
      untilNextYear = 12 + diff
  in if diff >= 0
       then Months (toInteger diff)
       else Months (toInteger untilNextYear)
diffToNextMatch (MonthlyInterval interval) startDate currentDate =
  let monthDiff = month currentDate - month startDate
      yearDiff = year currentDate - year startDate
      diff = monthDiff + (yearDiff * 12)
      offset = diff `mod` interval
  in if offset == 0
       then Months 0
       else Months (toInteger (interval - offset))
diffToNextMatch (WeeklyInterval interval) startDate currentDate =
  let startWeekBegin =
        minusInterval startDate $
        Days $ (toInteger . weekdayNumber . dateWeekDay) startDate
      currentWeekBegin =
        minusInterval currentDate $
        Days $ (toInteger . weekdayNumber . dateWeekDay) currentDate
      diff = datesDifference currentWeekBegin startWeekBegin
      offset = (diff `div` 7) `mod` toInteger interval
  in if offset == 0
       then Weeks 0
       else Weeks (toInteger interval - offset)
diffToNextMatch (YearlyInterval interval) startDate currentDate =
  let diff = year currentDate - year startDate
      offset = diff `mod` interval
  in if offset == 0
       then Years 0
       else Years (toInteger (interval - offset))
diffToNextMatch (DayOfWeek day) _startDate currentDate =
  let weekday = fromEnum day
      currentWeekday = fromEnum $ dateWeekDay currentDate
      diff =
        if weekday >= currentWeekday
          then weekday - currentWeekday
          else 7 - currentWeekday + weekday
  in Days (toInteger diff)

daysInMonth :: DateTime -> Int
daysInMonth DateTime {year, month} =
  monthLength (isLeapYear $ toInteger year) month

nextDay :: DateTime -> DateTime
nextDay d = addInterval d (Days 1)

prevDay :: DateTime -> DateTime
prevDay d = minusInterval d (Days 1)

isZero :: DateInterval -> Bool
isZero (Days x) = x == 0
isZero (Weeks x) = x == 0
isZero (Months x) = x == 0
isZero (Years x) = x == 0

mapMinimum :: (Foldable t, Ord b) => (a -> b) -> t a -> a
mapMinimum f = minimumBy (\x y -> compare (f x) (f y))
