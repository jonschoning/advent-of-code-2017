{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day11 where

import Data.List
import Data.Foldable
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

import Debug.Trace

data Cube =
  Cube !Int -- ^ dx
       !Int -- ^ dy
       !Int -- ^ dz
  deriving (Show)

cube_directions :: M.Map String Cube
cube_directions = M.fromList
  [ ("se", Cube 1 (-1) 0)
  , ("ne", Cube 1 0 (-1))
  , ("n" , Cube 0 1 (-1))
  , ("nw", Cube (-1) 1 0)
  , ("sw", Cube (-1) 0 1)
  , ("s" , Cube 0 (-1) 1)
  ]

cube_distance (Cube ax ay az) (Cube bx by bz) =
    maximum [abs(ax - bx), abs(ay - by), abs(az - bz)]

origin :: Cube
origin = Cube 0 0 0

-- | Part one
p1 :: B8.ByteString -> Int
p1 = cube_distance origin . foldl' move origin . readLines
  where
    move (Cube x y z) s =
      let Just (Cube dx dy dz) = M.lookup s cube_directions
      in Cube (x+dx) (y+dy) (z+dz)

-- | Part two
p2 :: B8.ByteString -> Int
p2 _ = 0

readLines :: B8.ByteString -> [String]
readLines =
  (fmap B8.unpack . B8.split ',')
  . head
  . B8.lines
