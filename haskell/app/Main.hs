{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B8

import qualified AOC.Day1 as D1
import qualified AOC.Day2 as D2
import qualified AOC.Day3 as D3
import qualified AOC.Day4 as D4
import qualified AOC.Day5 as D5
import qualified AOC.Day6 as D6

main :: IO ()
main = do
  (day:part:rest) <- getArgs
  case (day, part) of
    ("1", "1") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D1.p1fold input
      putStrLn $ show $ D1.p1zip input
    ("1", "2") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D1.p2zip input
    ("2", "1") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D2.p1 input
    ("2", "2") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D2.p2 input
    ("3", "1") -> do
      let (inputfile:_) = rest
      Just (input, _) <- B8.readInt <$> B8.readFile inputfile
      putStrLn $ show $ D3.p1 input
    ("3", "2") -> do
      let (inputfile:_) = rest
      Just (input, _) <- B8.readInt <$> B8.readFile inputfile
      putStrLn $ show $ D3.p2 input
    ("4", "1") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D4.p1 input
    ("4", "2") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D4.p2 input
    ("5", "1") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D5.p1 input
    ("5", "2") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D5.p2 input
    ("6", "1") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D6.p1 input
    ("6", "2") -> do
      let (inputfile:_) = rest
      input <- B8.readFile inputfile
      putStrLn $ show $ D6.p2 input
    _ -> do
      putStrLn "TODO"
