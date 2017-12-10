{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Day10 where

import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V

import Debug.Trace

initL :: V.Vector Int
initL = V.fromList [0..4]
-- initL = V.fromList [0..255]

-- | Part one
p1 :: B8.ByteString -> Int
p1 _ = 0

p2 :: B8.ByteString -> Int
p2 _ = 0


readLines :: B8.ByteString -> V.Vector Int
readLines =
  V.fromList
  . fmap fst . catMaybes . fmap B8.readInt . B8.split ','
  . head
  . B8.lines
