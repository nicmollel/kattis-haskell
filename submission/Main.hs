-- https://open.kattis.com/problems/qaly
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> map (words >>> map (read @Double) >>> product) >>> sum >>> show
