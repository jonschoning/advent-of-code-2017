{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day10 where

import Control.Monad.ST (runST, ST)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List
import Data.Foldable
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Printf (printf)

-- | Part one
p1 :: B8.ByteString -> Int
p1 = check . sparse_hash . readInts
 where
  check (a:b:_) = a * b
  readInts =
    fmap fst . catMaybes . fmap B8.readInt . B8.split ',' . head . B8.lines

-- | Part two
p2 :: B8.ByteString -> String
p2 = knot_hash . head . B8.lines

-- * knot_hash

data KHR s =
  KHR !Int -- ^ pos
      !Int -- ^ skip
      !(MV.STVector s Int) -- ^ hash

knot_hash :: B8.ByteString -> String
knot_hash input =
  let readLengths = B8.foldr (\b a -> ord b : a) []
      salt = [17, 31, 73, 47, 23]
      length_seq = readLengths input ++ salt
      expand = concat . replicate 64
      dense_hash = foldl1' xor <$> chunksOf 16 (sparse_hash (expand length_seq))
  in concatMap (printf "%02x") dense_hash

sparse_hash :: [Int] -> [Int]
sparse_hash length_seq = V.toList $ runST $ do
  khr           <- init_khr
  KHR _ _ hash' <- foldlM kh_round khr length_seq
  V.unsafeFreeze hash'

kh_round :: KHR s -> Int -> ST s (KHR s)
kh_round (KHR pos skip hash) n = do
  let ixs = (`rem` size_h) <$> [pos .. pos + n - 1]
  xs <- traverse hRead ixs
  traverse_ hWrite $ zip ixs (reverse xs)
  pure $ KHR (pos + skip + n `rem` size_h) (skip + 1) hash
  where
    hRead = MV.unsafeRead hash
    hWrite = uncurry (MV.unsafeWrite hash)

init_khr :: ST s (KHR s)
init_khr = fmap (KHR 0 0) (V.thaw (V.fromList [0 .. size_h - 1]))

size_h :: Int
size_h = 256
