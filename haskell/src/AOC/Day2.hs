module AOC.Day2 where

import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8

-- * Part One

{-|
The spreadsheet consists of rows of apparently-random numbers. To make
sure the recovery process is on the right track, they need you to
calculate the spreadsheet's checksum. For each row, determine the
difference between the largest value and the smallest value; the
checksum is the sum of all of these differences.
-}

p1 :: B8.ByteString -> Int
p1 = do
  sum
    . fmap checksum
    . readLines
  where
    checksum :: [Int] -> Int
    checksum xs = maximum xs - minimum xs

-- * Part Two

{-|
It sounds like the goal is to find the only two numbers in each row
where one evenly divides the other - that is, where the result of the
division operation is a whole number. They would like you to find
those numbers on each line, divide them, and add up each line's
result.
-}

p2 :: B8.ByteString -> Int
p2 = do
  sum
    . fmap divideResult
    . readLines
  where
    divideResult :: [Int] -> Int
    divideResult xs = maximum xs - minimum xs


-- * Utils

readLines :: B8.ByteString -> [[Int]]
readLines =
  fmap (fmap fst . catMaybes . fmap B8.readInt . B8.split '\t')
  . B8.lines
