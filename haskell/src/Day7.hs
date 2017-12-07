{-# LANGUAGE BangPatterns #-}

module Day7 where

import Data.Maybe
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as B8

-- import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as M

import Debug.Trace

-- * Part One

{-|
-}

data Parsed = Parsed String Int [String] deriving (Show)
-- |
--
--
p1 :: B8.ByteString -> String
p1 input = do
  traceShow (take 5 $ readLines $ input) "B"

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
parseLine (n:w:_:rest) = Parsed n (read $ filter isDigit w) (fmap (filter isAlphaNum) rest)
parseLine s = error (concat s)

readLines :: B8.ByteString -> [Parsed]
readLines =
  fmap (parseLine . words)
  . lines
  . B8.unpack
