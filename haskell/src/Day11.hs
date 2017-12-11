{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Data.List
import Data.Monoid
import Data.Maybe
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

data Cube =
  Cube !Int -- ^ x
       !Int -- ^ y
       !Int -- ^ z
  deriving (Show)

instance Num Cube where
  Cube ax ay az + Cube bx by bz = Cube (ax + bx) (ay + by) (az + bz)
  Cube ax ay az * Cube bx by bz = Cube (ax * bx) (ay * by) (az * bz)
  abs (Cube x y z) = Cube (abs x) (abs y) (abs z)
  negate (Cube x y z) =  Cube (-x) (-y) (-z)
  signum = error "NA"
  fromInteger = error "NA"

toListCube :: Cube -> [Int]
toListCube (Cube x y z) = [x, y, z]

directions :: M.Map B8.ByteString Cube
directions = M.fromList
  [ ("se", Cube 1 (-1) 0)
  , ("ne", Cube 1 0 (-1))
  , ("n" , Cube 0 1 (-1))
  , ("nw", Cube (-1) 1 0)
  , ("sw", Cube (-1) 0 1)
  , ("s" , Cube 0 (-1) 1)
  ]

distance :: Cube -> Cube -> Int
distance a b = maximum (toListCube (abs (a - b)))

move :: Cube -> B8.ByteString -> Cube
move a = (a +) . fromJust . flip M.lookup directions

origin :: Cube
origin = Cube 0 0 0

-- | Part one
p1 :: B8.ByteString -> Int
p1 = distance origin . foldl' move origin . readLines

-- | Part two
p2 :: B8.ByteString -> Int
p2 = maximum . fmap (distance origin) . scanl' move origin . readLines

-- * Utils

readLines :: B8.ByteString -> [B8.ByteString]
readLines = B8.split ',' . head . B8.lines
