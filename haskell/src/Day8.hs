{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Char
import Data.Either.Combinators
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

import Debug.Trace

-- $setup
-- >>> :set -XOverloadedStrings

-- * Part One

-- |
p1 :: B8.ByteString -> Int
p1 _ = 0

-- * Part Two

-- |
--
p2 :: B8.ByteString -> Int
p2 _ = 0

-- * Utils

readLines :: B8.ByteString -> [[String]]
readLines =
  fmap (words)
  . lines
  . B8.unpack
