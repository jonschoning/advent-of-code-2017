{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Criterion (nf, benchmark)
import Control.DeepSeq (NFData, rnf)
import qualified Data.ByteString.Char8 as B8
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import qualified Day9 as D9
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12

data AnyShow = forall a. (NFData a, Show a) => S a 
instance Show AnyShow where showsPrec p (S a) = showsPrec p a
instance NFData AnyShow where rnf (S a) = rnf a

main :: IO ()
main = do
  (cmd:day:part:inputfile:_) <- getArgs
  let dp = (day, part)
  input <- B8.readFile inputfile
  case cmd of
    "b" -> benchmark (nf (go dp) input)
    "p" -> print ((go dp) input)
    _   -> error "invalid cmd"
 where
  go = \case
    ("1" , "1") -> S . D1.p1zip
    ("1" , "2") -> S . D1.p2zip
    ("2" , "1") -> S . D2.p1
    ("2" , "2") -> S . D2.p2
    ("3" , "1") -> S . D3.p1
    ("3" , "2") -> S . D3.p2
    ("4" , "1") -> S . D4.p1
    ("4" , "2") -> S . D4.p2
    ("5" , "1") -> S . D5.p1
    ("5" , "2") -> S . D5.p2
    ("6" , "1") -> S . D6.p1
    ("6" , "2") -> S . D6.p2
    ("7" , "1") -> S . D7.p1
    ("7" , "2") -> S . D7.p2
    ("8" , "1") -> S . D8.p1
    ("8" , "2") -> S . D8.p2
    ("9" , "1") -> S . D9.p1
    ("9" , "2") -> S . D9.p2
    ("10", "1") -> S . D10.p1
    ("10", "2") -> S . D10.p2
    ("11", "1") -> S . D11.p1
    ("11", "2") -> S . D11.p2
    ("12", "1") -> S . D12.p1
    ("12", "2") -> S . D12.p2
    _           -> error "invalid arguments"
