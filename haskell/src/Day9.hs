{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Day9 where

import Data.Maybe (catMaybes)
import Data.List (foldl')
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8

data Stream
  = Group !Int
          [Stream]
  | Garbage !Int
  deriving (Show)

pGroup :: Int -> A.Parser Stream
pGroup i = do
  A.char '{'
  inner <-
    A.sepBy
      (A.choice [pGroup (i + 1), pGarbage])
      (A.char ',')
  A.char '}'
  pure $ Group i inner

pGarbage :: A.Parser Stream
pGarbage = do
  A.char '<'
  inner <-
    A.many' $
    A.choice [Nothing <$ (A.char '!' *> A.anyChar), Just <$> A.notChar '>']
  A.char '>'
  pure $ Garbage (length (catMaybes inner))

parseTopGroup :: (Int -> [Stream] -> Int) -> B8.ByteString -> Int
parseTopGroup f =
  either error (\(Group s inner) -> f s inner) . A.parseOnly (pGroup 1)

-- | Part one
p1 :: B8.ByteString -> Int
p1 = parseTopGroup (foldl' scoreGroup)
  where scoreGroup a (Garbage _) = a
        scoreGroup a (Group s inner) = a + foldl' scoreGroup s inner

p2 :: B8.ByteString -> Int
p2 = parseTopGroup (\_ -> foldl' scoreGarbage 0)
  where scoreGarbage a (Garbage s) = a + s
        scoreGarbage a (Group _ inner) = a + foldl' scoreGarbage 0 inner
