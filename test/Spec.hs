module Main where

import Test.Hspec
import Utils (orthodoxEaster, convertDate)
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
