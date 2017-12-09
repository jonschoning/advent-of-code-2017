{-# LANGUAGE OverloadedStrings #-}

module Day9Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified Day9 as D9

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1/score" $ do
      it "{}" $ D9.p1 "{}" `shouldBe` 1
      it "{{{}}}" $ D9.p1 "{{{}}}" `shouldBe` 6
      it "{{},{}}" $ D9.p1 "{{},{}}" `shouldBe` 5
      it "{{{},{},{{}}}}" $ D9.p1 "{{{},{},{{}}}}" `shouldBe` 16
      it "{<a>,<a>,<a>,<a>}" $ D9.p1 "{<a>,<a>,<a>,<a>}" `shouldBe` 1
      it "{{<ab>},{<ab>},{<ab>},{<ab>}}" $ D9.p1 "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
      it "{{<!!>},{<!!>},{<!!>},{<!!>}}" $ D9.p1 "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
      it "{{<a!>},{<a!>},{<a!>},{<ab>}}" $ D9.p1 "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
      it "input/day9.txt -> 20530" $ do
        input <- B8.readFile "input/day9.txt"
        D9.p1 input `shouldBe` 20530
    describe "Part2" $ do
      it "input/day9.txt -> 9978" $ do
        input <- B8.readFile "input/day9.txt"
        D9.p2 input `shouldBe` 9978
