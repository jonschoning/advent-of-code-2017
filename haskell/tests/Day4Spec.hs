{-# LANGUAGE OverloadedStrings #-}

module Day4Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified AOC.Day4 as D4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "" $ do
        D4.p1 "" `shouldBe` 0
      it "input/day4.txt" $ do
        input <- B8.readFile "input/day4.txt"
        D4.p1 input `shouldBe` 0

    describe "Part2" $ do
      it "input/day4.txt" $ do
        input <- B8.readFile "input/day4.txt"
        D4.p2 input `shouldBe` 0
