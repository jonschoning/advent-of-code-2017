{-# LANGUAGE BangPatterns #-}

module AOC.Day1 where

import Data.Char (digitToInt)
import qualified Data.ByteString.Char8 as B8

-- * Part One

{-|
The captcha requires you to review a sequence of digits (your puzzle
input) and find the sum of all digits that match the next digit in the
list. The list is circular, so the digit after the last digit is the
first digit in the list.
-}

p1fold :: B8.ByteString -> Int
p1fold input = fst $ B8.foldl' go_f (0, B8.last input) input
  where
    go_f (!acc, lst_c) c =
      if lst_c == c
        then (acc + digitToInt c, c)
        else (acc, c)

p1zip :: B8.ByteString -> Int
p1zip input =
  sum
    . fmap (digitToInt . fst)
    . filter (uncurry (==))
    $ B8.zip input (B8.tail input `B8.snoc` B8.head input)


-- * Part Two

{-|
Now, instead of considering the next digit, it wants you to consider
the digit halfway around the circular list. That is, if your list
contains 10 items, only include a digit in your sum if the digit 10/2
= 5 steps forward matches it. Fortunately, your list has an even
number of elements.
-}

p2zip :: B8.ByteString -> Int
p2zip input = do
  (2 *)
    . sum
    . fmap (digitToInt . fst)
    . filter (uncurry (==))
    . uncurry B8.zip
    $ B8.splitAt (B8.length input `div` 2) input
