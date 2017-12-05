{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AOC.Day5 where

import Data.Maybe
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.ByteString.Char8 as B8

-- * Part One

{-|
An urgent interrupt arrives from the CPU: it's trapped in a maze of
jump instructions, and it would like assistance from any programs with
spare cycles to help find the exit.

The message includes a list of the offsets for each jump. Jumps are
relative: -1 moves to the previous instruction, and 2 skips the next
one. Start at the first instruction in the list. The goal is to follow
the jumps until one leads outside the list.

In addition, these instructions are a little strange; after each jump,
the offset of that instruction increases by 1. So, if you come across
an offset of 3, you would move three instructions forward, but change
it to a 4 for the next time it is encountered.

How many steps does it take to reach the exit?
-}

p1 :: B8.ByteString -> Int
p1 input = runST $ V.thaw (readLines input) >>= go succ 0 0 

go newoffset !n !pos !v = do
    if pos < 0 || pos >= MV.length v
    then pure n
    else do
        offset <- MV.read v pos
        MV.write v pos (newoffset offset)
        go newoffset (n + 1) (pos + offset) v
{-# INLINE go #-}

-- * Part Two

{-|
Now, the jumps are even stranger: after each jump, if the offset was
three or more, instead decrease it by 1. Otherwise, increase it by 1
as before.

Using this rule with the above example, the process now takes 10
steps, and the offset values after finding the exit are left as 2 3 2
3 -1.

How many steps does it now take to reach the exit?
-}

p2 :: B8.ByteString -> Int
p2 input = runST $ V.thaw (readLines input) >>= go newoffset 0 0
  where
    newoffset o = if o >= 3 then pred o else succ o

-- (if offset >= 3 then offset-1 else offset+1)
-- * Utils

readLines :: B8.ByteString -> V.Vector Int
readLines =
  V.fromList
  . fmap (fst . fromJust . B8.readInt)
  . B8.lines
