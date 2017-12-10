{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Day10 where

import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

import Debug.Trace

init_s:: V.Vector Int
init_s = V.fromList [0..255]

size_s :: Int
size_s = V.length init_s

-- | Part one
p1 :: B8.ByteString -> Int
p1 = finish . foldl' go (0, 0, init_s) . readInts
  where
    finish (_, _, s) = (s V.! 0) * (s V.! 1)
    go (pos, skip, s) l = do
      let ixs = (`rem` size_s) <$> [pos,pos+1..pos+l-1]
          s' = s V.// zip ixs (reverse $ (s V.!) <$> ixs)
          pos' = (pos + l + skip) `rem` size_s
          skip' = skip + 1
          -- s'' = traceShow (pos',skip',l,s', ixs) s'
      (pos', skip', s')

p2 :: B8.ByteString -> Int
p2 _ = 0


-- * Utils

readInts :: B8.ByteString -> [Int]
readInts =
  fmap fst . catMaybes . fmap B8.readInt . B8.split ','
  . head
  . B8.lines
