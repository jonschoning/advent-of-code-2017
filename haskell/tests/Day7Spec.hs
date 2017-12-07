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
      it " -> 1" $ do
        let input = ""
        D7.p1 input `shouldBe` 1
      -- it "input/day7.txt -> 3157" $ do
      --   input <- B8.readFile "input/day7.txt"
      --   D7.p1 input `shouldBe` 3157

    -- describe "Part2" $ do
    --   it "0 2 7 0 -> 4" $ do
    --     let input = "0\t2\t7\t0\n"
    --     D7.p2 input `shouldBe` 4
    --   it "input/day7.txt -> 1710" $ do
    --     input <- B8.readFile "input/day7.txt"
    --     D7.p2 input `shouldBe` 1710
