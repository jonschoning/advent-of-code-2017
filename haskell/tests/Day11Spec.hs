{-# LANGUAGE OverloadedStrings #-}

module Day11Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day11 as D11

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "ne,ne,ne" $ D11.p1 "ne,ne,ne\n" `shouldBe` 3
      it "ne,ne,sw,sw" $ D11.p1 "ne,ne,sw,sw\n" `shouldBe` 0
      it "ne,ne,s,s" $ D11.p1 "ne,ne,s,s\n" `shouldBe` 2
      it "se,sw,se,sw,sw" $ D11.p1 "se,sw,se,sw,sw\n" `shouldBe` 3
      -- it "input/day11.txt -> 1" $ do
      --   input <- B8.readFile "input/day11.txt"
      --   D11.p1 input `shouldBe` 1
    -- describe "Part2" $ do
    --   it "input/day11.txt -> 1" $ do
    --     input <- B8.readFile "input/day11.txt"
    --     D11.p2 input `shouldBe` 1
