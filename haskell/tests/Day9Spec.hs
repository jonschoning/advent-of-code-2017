{-# LANGUAGE OverloadedStrings #-}

module Day9Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day9 as D9

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input/day9.txt -> 1" $ do
        input <- B8.readFile "input/day9.txt"
        D9.p1 input `shouldBe` 1
    describe "Part2" $ do
      it "input/day9.txt -> 1" $ do
        input <- B8.readFile "input/day9.txt"
        D9.p2 input `shouldBe` 1
