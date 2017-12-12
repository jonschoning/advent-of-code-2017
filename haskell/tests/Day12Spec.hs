{-# LANGUAGE OverloadedStrings #-}

module Day12Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day12 as D12

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input/day12.txt -> 169" $ do
        input <- B8.readFile "input/day12.txt"
        D12.p1 input `shouldBe` 169
    describe "Part2" $ do
      it "input/day12.txt -> 179" $ do
        input <- B8.readFile "input/day12.txt"
        D12.p2 input `shouldBe` 179
