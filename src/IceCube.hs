{-# LANGUAGE NamedFieldPuns #-}

module IceCube
  ( mkSchedule
  , Schedule
  , Rule(..)
  , occurrences
  ) where

import Data.Dates
       (DateInterval(..), DateTime(DateTime), WeekDay, weekdayNumber)
import qualified Data.Dates as D
       (addInterval, dateWeekDay, datesDifference, lastMonday,
        minusInterval)

import Data.Foldable (minimumBy)
import Data.Maybe
import Data.Time.Calendar (isLeapYear)
import Data.Time.Calendar.MonthDay (monthLength)

-- Date is a restricted version of DateTime with the hour, minute and second
-- fields constrained to zero
data Date = Date
  { year :: Int
  , month :: Int
  , day :: Int
  } deriving (Eq, Show, Ord)

toDate :: DateTime -> Date
toDate (DateTime y m d _ _ _) = Date y m d

fromDate :: Date -> DateTime
fromDate (Date y m d) = DateTime y m d 0 0 0

-- Here we redefine functions from Data.Dates to act on Date rather than
-- DateTime
addInterval :: Date -> DateInterval -> Date
addInterval d i = toDate $ D.addInterval (fromDate d) i

minusInterval :: Date -> DateInterval -> Date
minusInterval d i = toDate $ D.minusInterval (fromDate d) i

dateWeekDay :: Date -> WeekDay
dateWeekDay = D.dateWeekDay . fromDate

datesDifference :: Date -> Date -> Integer
datesDifference d1 d2 = D.datesDifference (fromDate d1) (fromDate d2)

lastMonday :: Date -> Date
lastMonday = toDate . D.lastMonday . fromDate

data Schedule = Schedule
  { startDate :: Date
  , endDate :: Maybe Date
  , rules :: [Rule]
  } deriving (Show)

-- |Create a new schedule
--
-- @
-- mkSchedule (DateTime 2000 1 1 0 0 0)
--            (DateTime 2001 1 1 0 0 0)
--            [MonthlyInterval 1, DayOfMonth 15]
-- @
mkSchedule ::
     DateTime -- ^ start date
  -> Maybe DateTime -- ^ end date
  -> [Rule] -- ^ recurrence rules
  -> Schedule
mkSchedule start end = Schedule (toDate start) (fmap toDate end)

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

-- |Get all occurrences for a schedule
occurrences :: Schedule -> [DateTime]
occurrences Schedule {startDate, endDate, rules} =
  map fromDate $ _occurrences rules startDate startDate endDate

-- TODO: could use whileJust here?
-- Because our date logic works by calculating the next valid date from the
-- current one, we start at one day before the start so that the start date
-- itself can be included as an occurrence.
_occurrences :: [Rule] -> Date -> Date -> Maybe Date -> [Date]
_occurrences rules current start end =
  drop 1 $ catMaybes $ takeWhile isJust $ iterate next (Just (prevDay current))
  where
    next :: Maybe Date -> Maybe Date
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
nextDate :: [Rule] -> Date -> Date -> Maybe Date -> Maybe Date
nextDate _ currentDate _ (Just endDate)
  | currentDate > endDate = Nothing
nextDate rules currentDate startDate endDate
  | all pairIsZero diffs = Just currentDate
  | otherwise = nextDate rules nextCandidate startDate endDate
  where
    diffs = map (\v -> (v, diffToNextMatch v startDate currentDate)) rules
    minDiff = fst $ minimumInterval (filter (not . pairIsZero) diffs)
    nextCandidate = nextMatch minDiff startDate currentDate
    pairIsZero :: (Rule, DateInterval) -> Bool
    pairIsZero = isZero . snd
    minimumInterval :: [(Rule, DateInterval)] -> (Rule, DateInterval)
    minimumInterval = minimumBy (\(_, i1) (_, i2) -> compareInterval i1 i2)

nextMatch :: Rule -> Date -> Date -> Date
nextMatch v@(MonthlyInterval _) start current = Date (year next) (month next) 1
  where
    next = addInterval current $ diffToNextMatch v start current
nextMatch v@(WeeklyInterval _) start current = lastMonday next
  where
    next = addInterval current $ diffToNextMatch v start current
nextMatch v@(YearlyInterval _) start current = Date (year next) 1 1
  where
    next = addInterval current $ diffToNextMatch v start current
nextMatch v start current =
  addInterval current $ diffToNextMatch v start current

diffToNextMatch :: Rule -> Date -> Date -> DateInterval
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

daysInMonth :: Date -> Int
daysInMonth (Date year month _) =
  monthLength (isLeapYear $ toInteger year) month

nextDay :: Date -> Date
nextDay d = d `addInterval` Days 1

prevDay :: Date -> Date
prevDay d = d `minusInterval` Days 1

isZero :: DateInterval -> Bool
isZero (Days x) = x == 0
isZero (Weeks x) = x == 0
isZero (Months x) = x == 0
isZero (Years x) = x == 0

compareInterval :: DateInterval -> DateInterval -> Ordering
compareInterval (Years a) (Years b) = compare a b
compareInterval (Years _) _ = GT
compareInterval (Months a) (Months b) = compare a b
compareInterval (Months _) _ = GT
compareInterval (Days a) (Days b) = compare a b
compareInterval (Days _) _ = GT
