module Day10 where

import Data.Char (ord)
import Numeric (showHex)
import Data.Bits (xor)
import Data.Maybe (catMaybes)
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

-- | Part one
p1 :: B8.ByteString -> Int
p1 = check . foldl' khRound init_khr . readInts
 where
  check (KHR _ _ s) = (s V.! 0) * (s V.! 1)
  readInts =
    fmap fst . catMaybes . fmap B8.readInt . B8.split ',' . head . B8.lines

-- | Part two
p2 :: B8.ByteString -> String
p2 = knot_hash . head . B8.lines

-- | knot_hash
knot_hash :: B8.ByteString -> String
knot_hash input = concatMap toHex dense_hash
 where
  readCodes   = B8.foldr (\b a -> ord b : a) [] 
  length_seq  = readCodes input ++ append_l
  expand      = concat . replicate 64
  sparse_hash = getHash $ foldl' khRound init_khr $ expand length_seq
  dense_hash  = foldr1 xor <$> chunksOf 16 sparse_hash

data KHRound =
  KHR !Int -- ^ pos
      !Int -- ^ skip
      !(V.Vector Int) -- ^ hash

khRound :: KHRound -> Int -> KHRound
khRound (KHR pos skip s) l =
  KHR
    ((pos + l + skip) `rem` size_h)
    (skip + 1)
    (s V.// zip ixs (reverse $ (s V.!) <$> ixs))
  where
    ixs = (`rem` size_h) <$> [pos,pos + 1 .. pos + l - 1]

init_khr :: KHRound
init_khr = KHR 0 0 (V.fromList [0 .. 255])

size_h :: Int
size_h = 256

append_l :: [Int]
append_l = [17, 31, 73, 47, 23]

getHash :: KHRound -> [Int]
getHash (KHR _ _ s) = V.toList s

toHex :: Int -> [Char]
toHex d = let h = showHex d "" in if length h == 1 then '0' : h else h
