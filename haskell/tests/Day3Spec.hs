{-# LANGUAGE OverloadedStrings #-}

module Day3Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified AOC.Day3 as D3

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "1 -> 0" $ do
        D3.p1 1 `shouldBe` 0
      it "12 -> 3" $ do
        D3.p1 12 `shouldBe` 3
      it "23 -> 2" $ do
        D3.p1 23 `shouldBe` 2
      it "1024 -> 31" $ do
        D3.p1 1024 `shouldBe` 31
      it "325489 -> 552" $ do
        Just (input, _) <- B8.readInt <$> B8.readFile "input/day3.txt"
        D3.p1 input `shouldBe` 552

    describe "Part2" $ do
      it "325489 -> 330785" $ do
        Just (input, _) <- B8.readInt <$> B8.readFile "input/day3.txt"
        D3.p2 input `shouldBe` 330785
