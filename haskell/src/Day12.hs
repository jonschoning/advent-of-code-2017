{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.List
import Data.Char
import Data.Monoid
import Data.Maybe
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set

-- | Part one
p1 :: B8.ByteString -> Int
p1 _ = 0

-- | Part two
p2 :: B8.ByteString -> Int
p2 _ = 0

-- * Utils

parseLine :: [String] -> (Int, Set Int)
parseLine (a:_:bs) =
  (read a, Set.fromList $ fmap (read . filter isDigit) bs)
parseLine s = error (concat s)

readLines :: B8.ByteString -> M.Map Int (Set Int)
readLines =
  M.fromList
  . fmap (parseLine . words)
  . lines
  . B8.unpack
