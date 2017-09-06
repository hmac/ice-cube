import Test.Hspec

import Data.Dates
import Lib

main :: IO ()
main =
  hspec $
  describe "occurrences" $
  let start = DateTime 2000 1 1 0 0 0
      end = DateTime 2020 1 1 0 0 0
  in do it "every friday" $
          let schedule = Schedule start end [DayOfWeek Friday]
              dates = take 5 $ occurrences schedule
              expected =
                [ DateTime 2000 1 7 0 0 0
                , DateTime 2000 1 14 0 0 0
                , DateTime 2000 1 21 0 0 0
                , DateTime 2000 1 28 0 0 0
                , DateTime 2000 2 4 0 0 0
                ]
          in dates `shouldBe` expected
        it "every friday the 4th" $
          let schedule = Schedule start end [DayOfWeek Friday, DayOfMonth 4]
              dates = take 3 $ occurrences schedule
              expected =
                [ DateTime 2000 2 4 0 0 0
                , DateTime 2000 8 4 0 0 0
                , DateTime 2001 5 4 0 0 0
                ]
          in dates `shouldBe` expected
        it "every 2 years on wednesdays in April" $
          let schedule =
                Schedule
                  start
                  end
                  [DayOfWeek Wednesday, MonthOfYear 4, YearlyInterval 2]
              dates = take 5 $ occurrences schedule
              expected =
                [ DateTime 2000 4 5 0 0 0
                , DateTime 2000 4 12 0 0 0
                , DateTime 2000 4 19 0 0 0
                , DateTime 2000 4 26 0 0 0
                , DateTime 2002 4 3 0 0 0
                ]
          in dates `shouldBe` expected
        it "returns a list of occurrences for a given schedule" $
          let rs = [MonthlyInterval 2, DayOfMonth 15]
              schedule = Schedule {startTime = start, endTime = end, rules = rs}
              dates = take 2 $ occurrences schedule
          in dates `shouldBe`
             [DateTime 2000 1 15 0 0 0, DateTime 2000 3 15 0 0 0]
        it "handles yearly intervals" $
          let schedule =
                Schedule
                  start
                  end
                  [YearlyInterval 2, MonthOfYear 1, DayOfMonth 1]
              dates = take 2 (occurrences schedule)
          in dates `shouldBe` [DateTime 2000 1 1 0 0 0, DateTime 2002 1 1 0 0 0]
        it "handles multiple rules" $
          let rs = [YearlyInterval 2, MonthlyInterval 5, DayOfMonth 10]
              schedule = Schedule start end rs
              dates = take 4 (occurrences schedule)
          in dates `shouldBe`
             [ DateTime 2000 1 10 0 0 0
             , DateTime 2000 6 10 0 0 0
             , DateTime 2000 11 10 0 0 0
             , DateTime 2002 2 10 0 0 0
             ]
