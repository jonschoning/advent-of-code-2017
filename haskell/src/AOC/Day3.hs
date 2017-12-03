module AOC.Day3 where

import Data.List (find)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid (getFirst, First(..))
import qualified Data.ByteString.Char8 as B8

import Debug.Trace

-- * Part One

{-|
Each square on the grid is allocated in a spiral pattern starting at a
location marked 1 and then counting up while spiraling outward. For
example, the first few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested
data must be carried back to square 1 (the location of the only access
port for this memory system) by programs that can only move up, down,
left, or right. They always take the shortest path: the Manhattan
Distance between the location of the data and square 1.

How many steps are required to carry the data from the square
identified in your puzzle input all the way to the access port?
1 -> 0
12 -> 3
23 -> 2
1024 -> 31
-}

p1 :: Int -> Int
p1 = undefined

-- * Part Two

{-|
-}

p2 :: Int -> Int
p2 = undefined

-- * Utils

readLines :: B8.ByteString -> [[Int]]
readLines =
  fmap (fmap fst . catMaybes . fmap B8.readInt . B8.split '\t')
  . B8.lines
