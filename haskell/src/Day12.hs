{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Day12 where

import Data.Char
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set


go :: M.Map Int (Set Int) -> Int -> Set Int
go m i = loop (Set.singleton 0) (Set.empty) m i
  where
    loop !grp !seen  _ !i | Set.member i seen = grp
    loop !grp !seen !m !i =
      let Just x = M.lookup i m
          xs = Set.toList x
          seen' = Set.insert i seen
          grps = fmap (loop grp seen' m) xs
      in Set.unions (x : grp : grps)

-- | Part one
p1 :: B8.ByteString -> Int
p1 input = finish $ go (readLines input) 0
  where
    finish grp = Set.size grp

-- | Part two
p2 :: B8.ByteString -> Int
p2 input =
  let m = readLines input
      grps = Set.map (go m) (Set.unions (M.elems m))
  in Set.size grps

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
