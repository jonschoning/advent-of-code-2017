{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as C8

import qualified AOC.Day1 as D1

main :: IO ()
main = do
  (day:part:rest) <- getArgs
  case (day, part) of
    ("1", "1") -> do
      let (inputfile:_) = rest
      input <- C8.readFile inputfile
      putStrLn $ show $ D1.p1fold input
      putStrLn $ show $ D1.p1zip input
    ("1", "2") -> do
      putStrLn "12TODO"
    _ -> do
      putStrLn "TODO"
