{-# LANGUAGE BangPatterns #-}

module AOC.Day6 where

import Data.Maybe
import qualified Data.ByteString.Char8 as B8

import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

-- * Part One

{-|
In this area, there are sixteen memory banks; each memory bank can
hold any number of blocks. The goal of the reallocation routine is to
balance the blocks between the memory banks.

The reallocation routine operates in cycles. In each cycle, it finds
the memory bank with the most blocks (ties won by the lowest-numbered
memory bank) and redistributes those blocks among the banks. To do
this, it removes all of the blocks from the selected bank, then moves
to the next (by index) memory bank and inserts one of the blocks. It
continues doing this until it runs out of blocks; if it reaches the
last memory bank, it wraps around to the first one.

The debugger would like to know how many redistributions can be done
before a blocks-in-banks configuration is produced that has been seen
before.

Given the initial block counts in your puzzle input, how many
redistribution cycles must be completed before a configuration is
produced that has been seen before?
-}

p1 :: B8.ByteString -> Int
p1 input = reallocation const 0 (M.empty) (readLines input)

reallocation
  :: (Int -> Int -> Int) -- ^ done
  -> Int -- ^ number of total reallocation cycles
  -> M.Map (V.Vector Int) Int -- ^ map of seen configurations w/ number of reallocation cycles
  -> V.Vector Int -- ^ current configuration
  -> Int -- ^ result
reallocation finish !n_cycles !seen !mem = do
  case M.lookup mem seen of
    Just n_last -> finish n_cycles n_last
    Nothing ->
      reallocation
        finish
        (succ n_cycles)
        (M.insert mem n_cycles seen)
        (balance mem)

balance :: V.Vector Int -> V.Vector Int
balance initial_mem =
  let max_pos = V.maxIndex initial_mem
  in go
       (pred $ initial_mem V.! max_pos)
       (next_pos initial_mem max_pos)
       (initial_mem V.// [(max_pos, 0)])
  where
    go blocks _ mem
      | blocks < 0 = mem
    go blocks pos mem =
      go
        (pred blocks)
        (next_pos mem pos)
        (mem V.// [(pos, succ (mem V.! pos))])

next_pos :: V.Vector Int -> Int -> Int
next_pos mem pos =
  if succ pos >= V.length mem
    then 0
    else succ pos


-- * Part Two

{-|
Out of curiosity, the debugger would also like to know the size of the
loop: starting from a state that has already been seen, how many block
redistribution cycles must be performed before that same state is seen
again?

How many cycles are in the infinite loop that arises from the
configuration in your puzzle input?
-}

p2 :: B8.ByteString -> Int
p2 input = reallocation (-) 0 (M.empty) (readLines input)

-- * Utils

readLines :: B8.ByteString -> V.Vector Int
readLines =
  V.fromList
  . fmap fst . catMaybes . fmap B8.readInt . B8.split '\t'
  . head
  . B8.lines
