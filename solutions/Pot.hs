module Pot where

-- https://open.kattis.com/problems/pot
import Control.Category ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> map (read >>> solve) >>> sum >>> show

solve :: Integer -> Integer
solve n = (n `div` 10) ^ (n `mod` 10)
