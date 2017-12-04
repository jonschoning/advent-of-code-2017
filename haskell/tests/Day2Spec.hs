{-# LANGUAGE OverloadedStrings #-}

module Day2Spec where

import Test.Hspec
import qualified Data.ByteString.Char8 as B8

import qualified AOC.Day2 as D2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "Part1" $ do
      it "input: \n5 1 9 5\n7 5 3\n2 4 6 8" $ do
        let input =
              flip B8.snoc '\n' $ B8.intercalate "\n" $ fmap (B8.intercalate "\t") $
                  [ ["5", "1", "9", "5"]
                  , ["7", "5", "3"     ]
                  , ["2", "4", "6", "8"]
                  ]
        D2.p1 input `shouldBe` 18
      it "input: file[input/day2.txt]" $ do
        input <- B8.readFile "input/day2.txt"
        D2.p1 input `shouldBe` 53978

    describe "Part2" $ do
      it "input: \n5 9 2 8\n9 4 7 3\n3 8 6 5" $ do
        let input =
              flip B8.snoc '\n' $ B8.intercalate "\n" $ fmap (B8.intercalate "\t") $
                  [ ["5", "9", "2", "8"]
                  , ["9", "4", "7", "3"]
                  , ["3", "8", "6", "5"]
                  ]
        D2.p2 input `shouldBe` 9
      it "input: file[input/day2.txt]" $ do
        input <- B8.readFile "input/day2.txt"
        D2.p2 input `shouldBe` 314
