{-# LANGUAGE BangPatterns #-}

module AOC.Day6 where

import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B8

import Control.Monad.ST (runST, ST)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

-- * Part One

{-|
-}

p1 :: B8.ByteString -> Int
p1 input = 0


-- * Part Two

{-|
-}

p2 :: B8.ByteString -> Int
p2 input = 0

-- * Utils

readLines :: B8.ByteString -> V.Vector Int
readLines =
  V.fromList
  . fmap (fst . fromJust . B8.readInt)
  . B8.lines
