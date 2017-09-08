import Test.Hspec

import Data.Dates
import IceCube

main :: IO ()
main =
  hspec $
  describe "occurrences" $
  let mkDate y m d = DateTime y m d 0 0 0
      start = mkDate 2000 1 1
      end = mkDate 2020 1 1
  in do it "every friday" $
          let schedule = mkSchedule start end [DayOfWeek Friday]
              dates = take 5 $ occurrences schedule
              expected =
                [ mkDate 2000 1 7
                , mkDate 2000 1 14
                , mkDate 2000 1 21
                , mkDate 2000 1 28
                , mkDate 2000 2 4
                ]
          in dates `shouldBe` expected
        it "every friday the 4th" $
          let schedule = mkSchedule start end [DayOfWeek Friday, DayOfMonth 4]
              dates = take 3 $ occurrences schedule
              expected = [mkDate 2000 2 4, mkDate 2000 8 4, mkDate 2001 5 4]
          in dates `shouldBe` expected
        it "every 2 years on wednesdays in April" $
          let schedule =
                mkSchedule
                  start
                  end
                  [DayOfWeek Wednesday, MonthOfYear 4, YearlyInterval 2]
              dates = take 5 $ occurrences schedule
              expected =
                [ mkDate 2000 4 5
                , mkDate 2000 4 12
                , mkDate 2000 4 19
                , mkDate 2000 4 26
                , mkDate 2002 4 3
                ]
          in dates `shouldBe` expected
        it "returns a list of occurrences for a given schedule" $
          let rs = [MonthlyInterval 2, DayOfMonth 15]
              schedule = mkSchedule start end rs
              dates = take 2 $ occurrences schedule
          in dates `shouldBe` [mkDate 2000 1 15, mkDate 2000 3 15]
        it "handles yearly intervals" $
          let schedule =
                mkSchedule
                  start
                  end
                  [YearlyInterval 2, MonthOfYear 1, DayOfMonth 1]
              dates = take 2 (occurrences schedule)
          in dates `shouldBe` [mkDate 2000 1 1, mkDate 2002 1 1]
        it "handles multiple rules" $
          let rs = [YearlyInterval 2, MonthlyInterval 5, DayOfMonth 10]
              schedule = mkSchedule start end rs
              dates = take 4 (occurrences schedule)
          in dates `shouldBe`
             [ mkDate 2000 1 10
             , mkDate 2000 6 10
             , mkDate 2000 11 10
             , mkDate 2002 2 10
             ]
