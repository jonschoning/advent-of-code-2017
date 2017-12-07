{-# LANGUAGE BangPatterns #-}

module Day7 where

import Data.Maybe
import qualified Data.ByteString.Char8 as B8

import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

-- * Part One

{-|
-}

-- |
--
--
p1 :: B8.ByteString -> Int
p1 _ = 0

-- * Part Two

{-|
-}

-- |
--
--
p2 :: B8.ByteString -> Int
p2 _ = 0

-- * Utils

readLines :: B8.ByteString -> V.Vector Int
readLines =
  V.fromList
  . fmap fst . catMaybes . fmap B8.readInt . B8.split '\t'
  . head
  . B8.lines
