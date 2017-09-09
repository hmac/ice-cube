import Test.Hspec

import Data.Dates
import IceCube

mkDate y m d = DateTime y m d 0 0 0

main :: IO ()
main =
  hspec $
  describe "occurrences" $ do
    context "finite schedules" $
      let start = mkDate 2000 1 1
          end = mkDate 2020 1 1
          schedule = mkSchedule start (Just end)
          dates rules = take 4 $ occurrences (schedule rules)
      in do it "every friday" $
              let rules = [DayOfWeek Friday]
                  expected =
                    [ mkDate 2000 1 7
                    , mkDate 2000 1 14
                    , mkDate 2000 1 21
                    , mkDate 2000 1 28
                    ]
              in dates rules `shouldBe` expected
            it "every friday the 4th" $
              let rules = [DayOfWeek Friday, DayOfMonth 4]
                  expected =
                    [ mkDate 2000 2 4
                    , mkDate 2000 8 4
                    , mkDate 2001 5 4
                    , mkDate 2002 1 4
                    ]
              in dates rules `shouldBe` expected
            it "every 2 years on wednesdays in April" $
              let rules = [DayOfWeek Wednesday, MonthOfYear 4, YearlyInterval 2]
                  expected =
                    [ mkDate 2000 4 5
                    , mkDate 2000 4 12
                    , mkDate 2000 4 19
                    , mkDate 2000 4 26
                    ]
              in dates rules `shouldBe` expected
            it "every 2 months on the 15th" $
              let rules = [MonthlyInterval 2, DayOfMonth 15]
              in dates rules `shouldBe`
                 [ mkDate 2000 1 15
                 , mkDate 2000 3 15
                 , mkDate 2000 5 15
                 , mkDate 2000 7 15
                 ]
            it "every 2 years on the 1st of January" $
              let rules = [YearlyInterval 2, MonthOfYear 1, DayOfMonth 1]
              in dates rules `shouldBe`
                 [ mkDate 2000 1 1
                 , mkDate 2002 1 1
                 , mkDate 2004 1 1
                 , mkDate 2006 1 1
                 ]
            it "every 2 years on the 10th of May" $
              let rules = [YearlyInterval 2, MonthlyInterval 5, DayOfMonth 10]
                  expected =
                    [ mkDate 2000 1 10
                    , mkDate 2000 6 10
                    , mkDate 2000 11 10
                    , mkDate 2002 2 10
                    ]
              in dates rules `shouldBe` expected
            it "every 7 years on the 1st of January" $
              let rules = [YearlyInterval 7, MonthOfYear 1, DayOfMonth 1]
                  expected = [mkDate 2000 1 1, mkDate 2007 1 1, mkDate 2014 1 1]
              in dates rules `shouldBe` expected
    context "infinite schedules" $
      let start = mkDate 2000 1 1
      in it "every month on the 15th" $
         let rules = [MonthlyInterval 1, DayOfMonth 15]
             schedule = mkSchedule start Nothing rules
             dates = take 121 $ occurrences schedule
         in last dates `shouldBe` mkDate 2010 1 15
