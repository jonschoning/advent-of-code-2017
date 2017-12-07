{-# LANGUAGE OverloadedStrings #-}

module Day7Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day7 as D7

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input/day7.txt -> eqgvf" $ do
        input <- B8.readFile "input/day7.txt"
        D7.p1 input `shouldBe` "eqgvf"

    describe "Part2" $ do
      it "input/day7.txt -> 757" $ do
        input <- B8.readFile "input/day7.txt"
        D7.p2 input `shouldBe` 757
