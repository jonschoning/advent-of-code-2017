{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Test.Hspec
import Test.HUnit.Lang
import qualified Data.ByteString.Char8 as C8

import qualified AOC.Day1 as D1

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
       input <- C8.readFile "input/day1.txt"
       D1.p1fold input `shouldBe` 1223
      it "input: file[input/day1.txt] (zip) " $ do
       input <- C8.readFile "input/day1.txt"
       D1.p1zip input `shouldBe` 1223
    describe "Day1/Part2" $ do
      it "" $ do
       assertFailure "todo"
