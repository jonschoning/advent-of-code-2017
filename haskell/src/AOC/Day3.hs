{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module AOC.Day3 where

import Data.List 
import Data.Maybe
import Control.Monad
import qualified Data.Map.Strict as M

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
-}

type Coord = (Int, Int)
data Move = R | U | L | D deriving (Eq, Show)

p1 :: Int -> Int
p1 = coordToDist . indexToCoord

initialOrigin :: Coord
initialOrigin = (0,0)

indexToCoord :: Int -> Coord
indexToCoord i = coords initialOrigin !! (i-1)

coordToDist :: Coord -> Int
coordToDist (q1, q2) = abs q1 + abs q2

moves :: [Move]
moves = join $ unfoldr move (0, D)
  where
    move (l, R) = Just (replicate  l    U, (l  , U))
    move (l, U) = Just (replicate (l+1) L, (l+1, L))
    move (l, L) = Just (replicate  l    D, (l  , D))
    move (l, D) = Just (replicate (l+1) R, (l+1, R))

coords :: Coord -> [Coord]
coords origin = scanl go origin moves
  where
    go (x, y) R = (x+1, y  )
    go (x, y) U = (x  , y+1)
    go (x, y) L = (x-1, y  )
    go (x, y) D = (x  , y-1)

-- * Part Two

{-|
As a stress test on the system, the programs here clear the grid and
then store the value 1 in square 1. Then, in the same allocation order
as shown above, they store the sum of the values in all adjacent
squares, including diagonals.

Square 1 starts with the value 1.
Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.

Once a square is written, its value does not change. Therefore, the

first few squares would receive the following values:

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...

What is the first value written that is larger than your puzzle input?
-}

p2 :: Int -> Int
p2 input = fromJust $ find (> input) $ gridValues

gridValues :: [Int]
gridValues = unfoldr step (initialGrid, tail (coords initialOrigin))
  where
    step (grid, (coord:cs)) = do
      let s = sum $ (\c -> M.findWithDefault 0 c grid) <$> neighbors coord
      Just (s, (M.insert coord s grid, cs))

initialGrid :: M.Map Coord Int
initialGrid = M.singleton initialOrigin 1

neighbors :: Coord -> [Coord]
neighbors = take 8 . tail . coords
