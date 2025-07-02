{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- https://open.kattis.com/problems/different
module Different where

import Control.Category ((>>>))

main :: IO ()
main =
  interact $
    lines >>> map (words >>> map read >>> solve >>> show) >>> unlines

solve :: [Integer] -> Integer
solve [a, b] = abs (a - b)
