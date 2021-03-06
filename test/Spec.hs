{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MACD
import Types
import Test.Hspec
import Data.Time
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "ta-lib test" $ do
        it "test macd" $ do
             -- print historyClosePrice
             -- print $ length historyClosePrice
             -- print $ length historyDate
             macd <- values <$> compute macdCfg historyData
             let macdPrices = fmap fst macd
                 macdSignals = fmap snd macd
             -- print macdPrices
             -- print macdSignals
             let output = zipWith (-) macdPrices macdSignals
                 output1 = dropWhile (< 0) output
                 output2 = takeWhile (> 0) output1
             print output
             -- print output1
             -- print output2

             length output1 `shouldBe` 18
             length output2 `shouldBe` 13

        it  "test rsi" $ do 
             output <- values <$> compute (RSI 6 24) historyData
             print output
             length output `shouldBe` 42

  where
    macdCfg = MACD 12 26 9
    parseStrToLocalTime x = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" x :: Maybe  LocalTime
    historyDate = map (fromJust . parseStrToLocalTime ) ["2013-2-19","2013-2-20","2013-2-21","2013-2-22","2013-2-25","2013-2-26","2013-2-27","2013-2-28","2013-3-1","2013-3-4","2013-3-5","2013-3-6","2013-3-7","2013-3-8","2013-3-11","2013-3-12","2013-3-13","2013-3-14","2013-3-15","2013-3-18","2013-3-19","2013-3-20","2013-3-21","2013-3-22","2013-3-25","2013-3-26","2013-3-27","2013-3-28","2013-4-1","2013-4-2","2013-4-3","2013-4-4","2013-4-5","2013-4-8","2013-4-9","2013-4-10","2013-4-11","2013-4-12","2013-4-15","2013-4-16","2013-4-17","2013-4-18","2013-4-19","2013-4-22","2013-4-23","2013-4-24","2013-4-25","2013-4-26","2013-4-29","2013-4-30","2013-5-1","2013-5-2","2013-5-3","2013-5-6","2013-5-7","2013-5-8","2013-5-9","2013-5-10","2013-5-13","2013-5-14","2013-5-15","2013-5-16","2013-5-17","2013-5-20","2013-5-21","2013-5-22"]
    historyClosePrice = [459.99,448.85,446.06,450.81,442.8,448.97,444.57,441.4,430.47,420.05,431.14,425.66,430.58,431.72,437.87,428.43,428.35,432.5,443.66,455.72,454.49,452.08,452.73,461.91,463.58,461.14,452.08,442.66,428.91,429.79,431.99,427.72,423.2,426.21,426.98,435.69,434.33,429.8,419.85,426.24,402.8,392.05,390.53,398.67,406.13,405.46,408.38,417.2,430.12,442.78,439.29,445.52,449.98,460.71,458.66,463.84,456.77,452.97,454.74,443.86,428.85,434.58,433.26,442.93,439.66,441.35]
    historyData = mkHistory historyDate historyClosePrice
