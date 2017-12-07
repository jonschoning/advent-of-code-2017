{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day7 where

import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B8

-- import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

import Debug.Trace

-- * Part One

{-|
-}

data Parsed =
  Parsed String -- ^ Name
         Int -- ^ Weight
         [String] -- ^ children
  deriving (Show)

    -- as = M.fromList $ fmap (\(Parsed n _ l) -> (n, l)) ps
    -- ws = M.fromList $ fmap (\(Parsed n w _) -> (n, w)) ps

-- |
--
--
p1 :: B8.ByteString -> String
p1 input = do
  findroot (head $ M.keys parents)
  where
    parsed = readLines input
    parents = foldl' insertParents M.empty parsed
    insertParents m (Parsed n _ c) = do
      m `M.union` M.fromList (fmap (, n) c)
    findroot k =
      case M.lookup k parents of
        Just n -> findroot n
        Nothing -> k

-- * Part Two

{-|
-}

-- |
--
--
p2 :: B8.ByteString -> Int
p2 _ = 0

-- * Utils

parseLine :: [String] -> Parsed
parseLine (n:w:[]) = Parsed n (read $ filter isDigit w) []
parseLine (n:w:_:children) = Parsed n (read $ filter isDigit w) (fmap (filter isAlphaNum) children)
parseLine s = error (concat s)

readLines :: B8.ByteString -> [Parsed]
readLines =
  fmap (parseLine . words)
  . lines
  . B8.unpack
