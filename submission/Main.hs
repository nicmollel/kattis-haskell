-- https://open.kattis.com/problems/romans
module Main where

import Control.Category ((>>>))

main :: IO ()
main =
  interact $
    read >>> solve >>> show

solve :: Double -> Integer
solve x = round (x * 1000 * 5280 / 4854)
