{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module AOC.Day4 where

import Data.List 
import qualified Data.ByteString.Char8 as B8
import Debug.Trace

-- * Part One

{-|
A new system policy has been put in place that requires all accounts
to use a passphrase instead of simply a password. A passphrase
consists of a series of words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

For example:

 - aa bb cc dd ee is valid.
 - aa bb cc dd aa is not valid - the word aa appears more than once.
 - aa bb cc dd aaa is valid - aa and aaa count as different words.

The system's full passphrase list is available as your puzzle
input. How many passphrases are valid?
-}

p1 :: B8.ByteString -> Int
p1 = length . filter isValid . readLines

-- better:
-- isValid line = nub line == line

isValid :: [String] -> Bool
isValid line = all (isUnique line) (zip line [0..])

isUnique :: [String] -> (String, Int) -> Bool
isUnique line (w, i) = null $ delete i $ findIndices (==w) line

-- * Part Two

{-|
For added security, yet another system policy has been put in
place. Now, a valid passphrase must contain no two words that are
anagrams of each other - that is, a passphrase is invalid if any
word's letters can be rearranged to form any other word in the
passphrase.

For example:

 - abcde fghij is a valid passphrase.
 - abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
 - a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
 - iiii oiii ooii oooi oooo is valid.
 - oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.

Under this new system policy, how many passphrases are valid?
-}

p2 :: B8.ByteString -> Int
p2 = length . filter isValid . fmap (fmap sort) . readLines

-- * Utils

readLines :: B8.ByteString -> [[String]]
readLines =
  fmap (fmap B8.unpack . B8.split ' ')
  . B8.lines
