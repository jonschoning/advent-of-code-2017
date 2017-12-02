{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.HUnit.Lang
import qualified Data.ByteString.Char8 as B8

import qualified AOC.Day1 as  D1
import qualified AOC.Day2 as D2

assertSuccess :: Expectation
assertSuccess = Success `shouldBe` Success

main :: IO ()
main = do
  hspec $ do

    describe "Day1/Part1" $ do
      it "input: 1122" $ do
        D1.p1fold "1122" `shouldBe` 3
        D1.p1zip "1122" `shouldBe` 3
      it "input: 1111" $ do
        D1.p1fold "1111" `shouldBe` 4
        D1.p1zip "1111" `shouldBe` 4
      it "input: 1234" $ do
        D1.p1fold "1234" `shouldBe` 0
        D1.p1zip "1234" `shouldBe` 0
      it "input: 91212129" $ do
        D1.p1fold "91212129" `shouldBe` 9
        D1.p1zip "91212129" `shouldBe` 9
      it "input: file[input/day1.txt] (fold) " $ do
        input <- B8.readFile "input/day1.txt"
        D1.p1fold input `shouldBe` 1223
      it "input: file[input/day1.txt] (zip) " $ do
        input <- B8.readFile "input/day1.txt"
        D1.p1zip input `shouldBe` 1223
    describe "Day1/Part2" $ do
      it "input: 1212" $ do
        D1.p2zip "1212" `shouldBe` 6
      it "input: 1221" $ do
        D1.p2zip "1221" `shouldBe` 0
      it "input: 123425" $ do
        D1.p2zip "123425" `shouldBe` 4
      it "input: 123123" $ do
        D1.p2zip "123123" `shouldBe` 12
      it "input: 12131415" $ do
        D1.p2zip "12131415" `shouldBe` 4
      it "input: file[input/day1.txt] (zip) " $ do
        input <- B8.readFile "input/day1.txt"
        D1.p2zip input `shouldBe` 1284

    describe "Day2/Part1" $ do
      it "input: \n5 1 9 5\n7 5 3\n2 4 6 8" $ do
        let input =
              flip B8.snoc '\n' $ B8.intercalate "\n" $ fmap (B8.intercalate "\t") $
                  [ ["5", "1", "9", "5"]
                  , ["7", "5", "3"     ]
                  , ["2", "4", "6", "8"]
                  ]
        D2.p1 input `shouldBe` 18
      it "input: file[input/day2.txt]" $ do
        input <- B8.readFile "input/day2.txt"
        D2.p1 input `shouldBe` 53978

    describe "Day2/Part2" $ do
      it "input: \n5 9 2 8\n9 4 7 3\n3 8 6 5" $ do
        let input =
              flip B8.snoc '\n' $ B8.intercalate "\n" $ fmap (B8.intercalate "\t") $
                  [ ["5", "9", "2", "8"]
                  , ["9", "4", "7", "3"]
                  , ["3", "8", "6", "5"]
                  ]
        D2.p2 input `shouldBe` 9
        assertFailure "todo"
      it "input: file[input/day2.txt]" $ do
        input <- B8.readFile "input/day2.txt"
        D2.p2 input `shouldBe` -1
