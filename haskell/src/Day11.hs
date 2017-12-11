{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day11 where

import Data.List
import Data.Foldable
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as V

-- ["n","ne","nw","s","se","sw"]

-- | Part one
p1 :: B8.ByteString -> Int
p1 _ = 0

-- | Part two
p2 :: B8.ByteString -> Int
p2 _ = 0

readLines :: B8.ByteString -> [String]
readLines =
  (fmap B8.unpack . B8.split ',')
  . head
  . B8.lines
