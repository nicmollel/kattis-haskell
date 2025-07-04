-- https://byorgey.github.io/blog/posts/2020/05/19/competitive-programming-in-haskell-sorting-tree-shapes.html
-- https://open.kattis.com/problems/ceiling
{-# LANGUAGE DeriveFunctor #-}

module Ceiling where

import Control.Arrow ((>>>))
import Data.List (group, sort)

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> map (words >>> map read) >>> solve >>> show

solve :: [[Int]] -> Int
solve = map (foldl' (flip put) Empty >>> (() <$)) >>> sort >>> group >>> length

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq, Ord, Functor)

put :: (Ord a) => a -> Tree a -> Tree a
put v Empty = Node v Empty Empty
put nv (Node v l r)
  | nv < v = Node v (put nv l) r
  | otherwise = Node v l (put nv r)
