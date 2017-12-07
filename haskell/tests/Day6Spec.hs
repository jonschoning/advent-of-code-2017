{-# LANGUAGE OverloadedStrings #-}

module Day6Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day6 as D6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "0 2 7 0 -> 5" $ do
        let input = "0\t2\t7\t0\n"
        D6.p1 input `shouldBe` 5
      it "input/day6.txt -> 3156" $ do
        input <- B8.readFile "input/day6.txt"
        D6.p1 input `shouldBe` 3156

    describe "Part2" $ do
      it "0 2 7 0 -> 4" $ do
        let input = "0\t2\t7\t0\n"
        D6.p2 input `shouldBe` 4
      it "input/day6.txt -> 1610" $ do
        input <- B8.readFile "input/day6.txt"
        D6.p2 input `shouldBe` 1610
