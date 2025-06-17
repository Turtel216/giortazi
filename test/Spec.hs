module Main where

import Test.Hspec
import Utils (orthodoxEaster, convertDate, fromToEasterToDay, intToDay)
import Data.Time.Calendar (fromGregorian)

main :: IO ()
main = hspec $ do
  describe "orthodoxEaster" $ do
    it "calculates the correct date for Orthodox Easter Sunday in 2023" $
      orthodoxEaster 2023 `shouldBe` fromGregorian 2023 4 16 -- Orthodox Easter in 2023

    it "calculates the correct date for Orthodox Easter Sunday in 2024" $
      orthodoxEaster 2024 `shouldBe` fromGregorian 2024 5 5 -- Orthodox Easter in 2024

    it "calculates the correct date for Orthodox Easter Sunday in 2025" $
      orthodoxEaster 2025 `shouldBe` fromGregorian 2025 4 20 -- Orthodox Easter in 2025

  describe "convertDate" $ do
    it "converts date from YYYY-MM-DD to DD/MM format" $
      convertDate "2023-04-16" `shouldBe` "16/04"

  describe "Converts days to easter to actual date string in the format of DD/MM" $ do
    it "convert day of easter 2025 to actual date string" $
      fromToEasterToDay 0 2025 `shouldBe` "20/04"

    it "convert day after easter 2025 to actual date string" $
      fromToEasterToDay 1 2025 `shouldBe` "21/04"

    it "convert day before easter 2025 to actual date string" $
      fromToEasterToDay (-1) 2025 `shouldBe` "19/04"

  describe "intToDate" $ do
    it "convert an Int Date which has a day and month less than 10 to a String DD/MM representation" $
      intToDay 1 1 `shouldBe` "01/01"

    it "convert an Int Date which has a day less than 10 and month greater than 10 to a String DD/MM representation" $
      intToDay 1 10 `shouldBe` "01/10"

    it "convert an Int Date which has a day greater than 10 and month less than 10 to a String DD/MM representation" $
      intToDay 10 1 `shouldBe` "10/01"

    it "convert an Int Date which has a day and month greater than 10 to a String DD/MM representation" $
      intToDay 10 10 `shouldBe` "10/10"
