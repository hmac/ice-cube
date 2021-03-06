# ice-cube

[![CircleCI](https://circleci.com/gh/hmac/ice-cube.svg?style=svg)](https://circleci.com/gh/hmac/ice-cube)

A port of the Ruby [ice_cube](https://github.com/seejohnrun/ice_cube) gem to
Haskell.

# Usage

ice-cube has a very small API. The date type it uses comes from the `Data.Dates`
package.

```haskell
import Data.Dates
import IceCube (mkSchedule, occurrences, Rule(..))
```

Use `mkSchedule` to create your schedule. `mkSchedule` takes a start date, an
optional end date and a list of recurrence rules.

```haskell
-- Create a schedule for the 15 of every month,
-- starting on Jan 1st 2017 and ending on Jan 1st 2018
let start = DateTime 2017 1 1 0 0 0
    end = DateTime 2018 1 1 0 0 0
    schedule = mkSchedule start (Just end) [DayOfMonth 15]

-- Create a schedule for the Fridays in February,
-- starting on Jan 1st 2017 and continuing forever
let start = DateTime 2017 1 1 0 0 0
    schedule = mkSchedule start Nothing [DayOfWeek Friday, MonthOfYear 2]
```

You can pass any number of rules to a schedule to create more complex recurrence
patterns.

```haskell
-- Every two weeks on Friday
[WeeklyInterval 2, DayOfWeek Friday]

-- Every other year on the 12th of March
[YearlyInterval 2, MonthOfYear 3, DayOfMonth 12]

-- Every day
[]
```

See the documentation for an exhaustive list of rules.

To get the dates for a schedule, use `occurrences`. If the schedule has no end
date, the returned list of dates will be infinite.

```haskell
let start = DateTime 2017 1 1 0 0 0 -- January 1st 2017
    end = DateTime 2018 1 1 0 0 0   -- January 1st 2018
    schedule = mkSchedule start (Just end) [DayOfMonth 15]
    first_date = take 1 $ occurrences schedule
```
