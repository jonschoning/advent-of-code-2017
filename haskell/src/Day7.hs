{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Day7 where

import Data.Char (isDigit, isAlphaNum)
import Data.Either.Combinators (fromLeft')
import Data.List (foldl', partition)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

import Debug.Trace

type SumWeight = Int

type Name = String

type UnbalancedInfo = [(SumWeight, Node)]

data Node =
  Node Name -- ^ Name
       Int -- ^ Weight
       [Name] -- ^ children
  deriving (Show)

-- * Part One

p1 :: B8.ByteString -> String
p1 input = do
  findroot (head $ M.keys parents)
  where
    parsed = readLines input
    parents = foldl' insertParents M.empty parsed
    insertParents m (Node n _ c) = do
      m `M.union` M.fromList (fmap (, n) c)
    findroot k =
      case M.lookup k parents of
        Just n -> findroot n
        Nothing -> k

-- * Part Two

p2 :: B8.ByteString -> Int
p2 input = calcCorrection . fromLeft' . findUnbalanced . get $ p1 input
  where
    get :: String -> Node
    get n = fromJust $ M.lookup n assocs

    assocs :: M.Map Name Node
    assocs = M.fromList $ fmap (\p@(Node n _ _) -> (n, p)) $ readLines input

    findUnbalanced :: Node -> Either UnbalancedInfo SumWeight
    findUnbalanced (Node _ w c) = do
      let cs = fmap get c
      wcs <- traverse findUnbalanced cs
      if all (== head wcs) wcs
        then Right (w + sum wcs)
        else Left (zip wcs cs)

    calcCorrection :: UnbalancedInfo -> Int
    calcCorrection u = do
        let (a, b) = partition (\p -> fst p == fst (head u)) u
            ((e, _):_, [(s, Node _ w _)]) = if length a == 1 then (b, a) else (a, b)
        e - s + w

-- * Utils

parseLine :: [String] -> Node
parseLine (n:w:[]) =
  Node n (read $ filter isDigit w) []
parseLine (n:w:_:c) =
  Node n (read $ filter isDigit w) (filter isAlphaNum <$> c)
parseLine s = error (concat s)

readLines :: B8.ByteString -> [Node]
readLines =
  fmap (parseLine . words)
  . lines
  . B8.unpack
