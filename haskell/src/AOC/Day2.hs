{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AOC.Day2 where

import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8

-- The spreadsheet consists of rows of apparently-random numbers. To make
-- sure the recovery process is on the right track, they need you to
-- calculate the spreadsheet's checksum. For each row, determine the
-- difference between the largest value and the smallest value; the
-- checksum is the sum of all of these differences.

p1 :: B8.ByteString -> Int
p1 input = do
  sum
    . fmap checksum
    . fmap (fmap fst . catMaybes . fmap B8.readInt . B8.split '\t')
    $ B8.lines input
  where
    checksum :: [Int] -> Int;
    checksum xs = maximum xs - minimum xs

