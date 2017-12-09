{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day9 where

import Data.List
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

import Debug.Trace

readLines :: B8.ByteString -> [Int]
readLines = fmap (parseLine . B8.words) . B8.lines

parseLine :: [B8.ByteString] -> Int
parseLine (i:[]) =
  readInt i
  where
    readInt = maybe (error "readInt") fst . B8.readInt

-- | Part one
p1 :: B8.ByteString -> Int
p1 _ = 0

-- | Part two
p2 :: B8.ByteString -> Int
p2 _ = 0
