{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Day8 where

import Data.List
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

type Reg = B8.ByteString
data Inst = Inst Reg (Int -> Int -> Int) Int Cond
data Cond = Cond Reg (Int -> Int -> Bool) Int

readLines :: B8.ByteString -> [Inst]
readLines = fmap (parseLine . B8.words) . B8.lines

parseLine :: [B8.ByteString] -> Inst
parseLine (reg:op:amt:_:cReg:cComp:cInt:[]) =
  Inst reg (readOp op) (readInt amt) (Cond cReg (readComp cComp) (readInt cInt))
  where
    readOp = \case
      "inc" -> (+)
      "dec" -> (-)
    readComp = \case
      "<" -> (<)
      "<=" -> (<=)
      "==" -> (==)
      "!=" -> (/=)
      ">" -> (>)
      ">=" -> (>=)
    readInt = maybe (error "readInt") fst . B8.readInt

runInst :: (Int, M.Map Reg Int) -> Inst -> (Int, M.Map Reg Int)
runInst (high, cpu) (Inst reg op amt (Cond cReg cComp cInt)) =
  if (get cReg `cComp` cInt)
    then let amt' = get reg `op` amt
         in (max amt' high, M.insert reg amt' cpu)
    else (high, cpu)
  where
    get r = M.findWithDefault 0 r cpu

-- | Part one
p1 :: B8.ByteString -> Int
p1 = maximum . snd . foldl' runInst (0, M.empty) . readLines

-- | Part two
p2 :: B8.ByteString -> Int
p2 = fst . foldl' runInst (0, M.empty) . readLines
