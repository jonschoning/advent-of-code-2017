{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day8 where

import Data.List
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

-- | parse input
readLines :: B8.ByteString -> [Inst]
readLines = fmap (parseLine . words) . lines . B8.unpack

-- | parse Instruction
parseLine :: [String] -> Inst
parseLine (reg:op:amt:_:cReg:cComp:cAmt:[]) =
  Inst reg (readOp op) (read amt) (Cond cReg (readComp cComp) (read cAmt))
parseLine s = error (concat s)

-- | Cpu
type Cpu = M.Map Reg Amt

-- | Register
type Reg = String

-- | Amount/Register Value
type Amt = Int

-- | Instruction
data Inst = Inst Reg Op Amt Cond

-- | Condition
data Cond = Cond Reg Comp Amt

-- | Operation
type Op = (Int -> Int -> Int)

readOp :: String -> Op
readOp = \case
  "inc" -> (+)
  "dec" -> (-)
  _    -> error "bad parse"

-- | Comparison
type Comp = (Int -> Int -> Bool)

readComp :: String -> Comp
readComp = \case
  "<"  -> (<)
  "<=" -> (<=)
  "==" -> (==)
  "!=" -> (/=)
  ">"  -> (>)
  ">=" -> (>=)
  _    -> error "bad parse"

-- | find the largest amount in any register
largestAmt :: Cpu -> Amt
largestAmt = M.foldl' max 0

-- | execute an instruction on the "cpu"
runInst :: Cpu -> Inst -> Cpu
runInst !cpu (Inst reg op amt (Cond cReg cComp cAmt)) =
  if (get cReg `cComp` cAmt)
    then M.insert reg (get reg `op` amt) cpu
    else cpu
  where
    get r = M.findWithDefault 0 r cpu

-- * Part One

p1 :: B8.ByteString -> Int
p1 = largestAmt . foldl' runInst M.empty . readLines

-- * Part Two

p2 :: B8.ByteString -> Int
p2 = maximum . tail . fmap largestAmt . scanl runInst M.empty . readLines
