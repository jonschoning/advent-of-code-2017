{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module AOC.Day4 where

import Data.List 
import Data.Maybe
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B8
import Debug.Trace

-- * Part One

{-|
A new system policy has been put in place that requires all accounts
to use a passphrase instead of simply a password. A passphrase
consists of a series of words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

For example:

aa bb cc dd ee is valid.
aa bb cc dd aa is not valid - the word aa appears more than once.
aa bb cc dd aaa is valid - aa and aaa count as different words.

The system's full passphrase list is available as your puzzle
input. How many passphrases are valid?
-}

p1 :: B8.ByteString -> Int
p1 _ = 1


-- * Part Two

{-|
-}

p2 :: B8.ByteString -> Int
p2 _ = 1
