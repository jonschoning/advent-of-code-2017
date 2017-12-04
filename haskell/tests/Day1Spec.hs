{-# LANGUAGE OverloadedStrings #-}

module Day1Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified AOC.Day1 as D1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
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
      it "input: file[input/day1.txt]" $ do
        input <- B8.readFile "input/day1.txt"
        D1.p1zip input `shouldBe` 1223

    describe "Part2" $ do
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
      it "input: file[input/day1.txt]" $ do
        input <- B8.readFile "input/day1.txt"
        D1.p2zip input `shouldBe` 1284
