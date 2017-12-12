{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Day12 where

import Data.Char
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set

-- | Part one
p1 :: B8.ByteString -> Int
p1 input = finish $ go (readLines input) 0
  where
    finish grp = Set.size grp

go :: M.Map Int (Set Int) -> Int -> Set Int
go m i = go' (Set.singleton 0) (Set.empty) m i

go' :: Set Int -> Set Int -> M.Map Int (Set Int) -> Int -> Set Int
go' !grp !seen !m !i = do
    if (Set.member i seen)
    then grp
    else case M.lookup i m of
            Just x -> do
                let xs = Set.toList x
                    seen' = Set.insert i seen
                    grps = fmap (\x' -> go' grp seen' m x') xs
                Set.unions (x : grp : grps)
            Nothing -> grp

-- | Part two
p2 :: B8.ByteString -> Int
p2 input =
  let m = readLines input
      grps = Set.map (go m) (Set.unions (M.elems m))
  in Set.size grps

-- * Utils

parseLine :: [String] -> (Int, Set Int)
parseLine (a:_:bs) =
  (read a, Set.fromList $ fmap (read . filter isDigit) bs)
parseLine s = error (concat s)

readLines :: B8.ByteString -> M.Map Int (Set Int)
readLines =
  M.fromList
  . fmap (parseLine . words)
  . lines
  . B8.unpack
