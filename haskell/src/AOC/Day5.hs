{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module AOC.Day5 where

import Data.List 
import Data.Maybe
import qualified Data.ByteString.Char8 as B8
import Debug.Trace

-- * Part One

{-|
-}

p1 :: B8.ByteString -> Int
p1 _ = 0

-- * Part Two

{-|
-}

p2 :: B8.ByteString -> Int
p2 _ = 0

-- * Utils

readLines :: B8.ByteString -> [Int]
readLines =
  fmap (fst . fromJust . B8.readInt)
  . B8.lines
