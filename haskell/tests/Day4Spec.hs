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
      it "input/day4.txt -> 325" $ do
        input <- B8.readFile "input/day4.txt"
        D4.p1 input `shouldBe` 325

    describe "Part2" $ do
      it "input/day4.txt -> 119" $ do
        input <- B8.readFile "input/day4.txt"
        D4.p2 input `shouldBe` 119
