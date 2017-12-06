{-# LANGUAGE BangPatterns #-}

module AOC.Day6 where

import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B8

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Set as S

import Debug.Trace

-- * Part One

{-|
-}

p1 :: B8.ByteString -> Int
p1 input = go (0 :: Int) (S.empty) (readLines input)
  where
    go !n !s !v = do
      if S.member v s
        then n
        else do
          let v' = balance v
          go (succ n) (S.insert v s) (v')
    balance v = do
      let i = V.maxIndex v
      let v' = V.unsafeUpd v [(i, 0)]
      balance' (pred $ v V.! i) (next i v') v'
    balance' sb _ v | sb < 0 = v
    balance' sb i v = do
      let v' = V.unsafeUpd v [(i, succ (v V.! i))]
      balance' (pred sb) (next i v)  v'
    next i v = 
      if succ i >= V.length v then 0 else succ i
      


-- * Part Two

{-|
-}

p2 :: B8.ByteString -> Int
p2 _ = 0

-- * Utils

readLines :: B8.ByteString -> V.Vector Int
readLines =
  V.fromList
  . fmap fst . catMaybes . fmap B8.readInt . B8.split '\t'
  . head
  . B8.lines
