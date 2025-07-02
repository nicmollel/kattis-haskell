-- https://open.kattis.com/problems/tarifa
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Category ((>>>))

main :: IO ()
main =
  interact $
    lines >>> map read >>> solve >>> show

solve :: [Integer] -> Integer
solve (x : n : ps) = x * (n + 1) - sum ps
