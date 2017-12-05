{-# LANGUAGE OverloadedStrings #-}

module Day5Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified AOC.Day5 as D5

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "[0 3 0 1 -3] -> 5" $ do
        let input = "0\n3\n0\n1\n-3"
        D5.p1 input `shouldBe` 5

    -- describe "Part2" $ do
    --   it "input/day5.txt -> 1" $ do
    --     input <- B8.readFile "input/day5.txt"
    --     D5.p2 input `shouldBe` 1
