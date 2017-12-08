{-# LANGUAGE OverloadedStrings #-}

module Day8Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day8 as D8

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input/day8.txt -> 4647" $ do
        input <- B8.readFile "input/day8.txt"
        D8.p1 input `shouldBe` 4647
    describe "Part2" $ do
      it "input/day8.txt -> 5590" $ do
        input <- B8.readFile "input/day8.txt"
        D8.p2 input `shouldBe` 5590
