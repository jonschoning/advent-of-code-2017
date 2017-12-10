{-# LANGUAGE OverloadedStrings #-}

module Day10Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day10 as D10

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input/day10.txt -> 1" $ do
        input <- B8.readFile "input/day10.txt"
        D10.p1 input `shouldBe` 1
    describe "Part2" $ do
      it "input/day10.txt -> 1" $ do
        input <- B8.readFile "input/day10.txt"
        D10.p2 input `shouldBe` 1
