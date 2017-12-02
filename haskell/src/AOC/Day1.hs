{-# LANGUAGE BangPatterns #-}

module AOC.Day1 where

import Data.Char (digitToInt)
import qualified Data.ByteString.Char8 as C8

p1fold :: C8.ByteString -> Int
p1fold input = fst $ C8.foldl' go_f (0, C8.last input) input
  where
    go_f (!acc, lst_c) c =
      if lst_c == c
        then (acc + digitToInt c, c)
        else (acc, c)

p1zip :: C8.ByteString -> Int
p1zip input =
  sum
    . fmap (digitToInt . fst)
    . filter (uncurry (==))
    $ C8.zip input (C8.tail input `C8.snoc` C8.head input)
