{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day8 where

import Data.List
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import Prelude hiding (LT, EQ, GT)

-- | parse input
readLines :: B8.ByteString -> [Inst]
readLines = fmap (parseLine . words) . lines . B8.unpack

-- | parse Instruction
parseLine :: [String] -> Inst
parseLine (reg:op:amt:_:cReg:cComp:cAmt:[]) =
  Inst reg (readOp op) (read amt) (Cond cReg (readComp cComp) (read cAmt))
parseLine s = error (concat s)

-- | Register
type Reg = String

-- | Amount/Regsiter Value
type Amt = Int

-- | Instruction
data Inst = Inst Reg Op Amt Cond deriving (Show, Eq)

-- | Condition
data Cond = Cond Reg Comp Amt deriving (Show, Eq)

-- | Operation
data Op = Inc | Dec deriving (Show, Eq)

readOp :: String -> Op
readOp = \case
  "inc" -> Inc
  "dec" -> Dec
  _    -> error "bad parse"

runOp :: Op -> (Int -> Int -> Int)
runOp = \case
  Inc -> (+)
  Dec -> (-)

-- | Comparison
data Comp = LT | LTEQ | EQ | NEQ | GT | GTEQ deriving (Show, Eq)

readComp :: String -> Comp
readComp = \case
  "<"  -> LT
  "<=" -> LTEQ
  "==" -> EQ
  "!=" -> NEQ
  ">"  -> GT
  ">=" -> GTEQ
  _    -> error "bad parse"

runComp :: Comp -> (Int -> Int -> Bool)
runComp = \case
  LT   -> (<)
  LTEQ -> (<=)
  EQ   -> (==)
  NEQ  -> (/=)
  GT   -> (>)
  GTEQ -> (>=)

-- | find the largest amount in any register
largestAmt :: M.Map Reg Amt -> Amt
largestAmt = maximum . fmap snd . M.toList

-- | execute an instruction on the "cpu"
runInst :: M.Map Reg Amt -> Inst -> M.Map Reg Amt
runInst !cpu (Inst reg op amt (Cond cReg cComp cAmt)) =
  if ((runComp cComp) (M.findWithDefault 0 cReg cpu) cAmt)
    then M.insert reg ((runOp op) (M.findWithDefault 0 reg cpu) amt) cpu
    else cpu

-- * Part One

p1 :: B8.ByteString -> Int
p1 = largestAmt . foldl' runInst M.empty . readLines

-- * Part Two

p2 :: B8.ByteString -> Int
p2 = maximum . tail . fmap largestAmt . scanl runInst M.empty . readLines
