{-# LANGUAGE OverloadedStrings #-}

module Day12Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day11 as D12

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input/day12.txt -> 877" $ do
        input <- B8.readFile "input/day12.txt"
        D12.p1 input `shouldBe` 877
    describe "Part2" $ do
      it "input/day12.txt -> 1622" $ do
        input <- B8.readFile "input/day12.txt"
        D12.p2 input `shouldBe` 1622
