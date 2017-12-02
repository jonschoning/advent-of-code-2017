{-# LANGUAGE BangPatterns #-}

module AOC.Day2 where

import Data.Char (digitToInt)
import qualified Data.ByteString.Char8 as B8

-- The spreadsheet consists of rows of apparently-random numbers. To make
-- sure the recovery process is on the right track, they need you to
-- calculate the spreadsheet's checksum. For each row, determine the
-- difference between the largest value and the smallest value; the
-- checksum is the sum of all of these differences.

p1 :: B8.ByteString -> Int
p1 input = 0
