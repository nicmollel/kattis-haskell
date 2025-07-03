-- https://byorgey.github.io/blog/posts/2019/10/12/competitive-programming-in-haskell-reading-large-inputs-with-bytestring.html
-- https://open.kattis.com/problems/armystrengthhard
-- https://open.kattis.com/problems/armystrengtheasy
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ArmyStrength where

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromJust)

main :: IO ()
main =
  C.interact $
    C.lines
      >>> drop 1
      >>> chunksOf 4
      >>> map (drop 2 >>> map (C.words >>> map readInt) >>> solve)
      >>> C.unlines
  where
    readInt = C.readInt >>> fromJust >>> fst

solve :: [[Int]] -> C.ByteString
solve [gz, mgz] = case compare (maximum gz) (maximum mgz) of
  LT -> C.pack "MechaGodzilla"
  _ -> C.pack "Godzilla"

-- | Split a list into chunks of size n.
--
-- >>> chunksOf 3 [1,2,3,4,5,6,7,8,9]

-- >>> chunksOf 3 [1,2,3,4,5]
--
-- >>> chunksOf 2 []
--
-- >>> chunksOf 1 [1,2,3]
--
-- >>> chunksOf 5 [1,2,3]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = go xs []
  where
    go [] acc = [reverse acc]
    go ys@(z : zs) acc
      | length acc == n = reverse acc : go ys []
      | otherwise = go zs (z : acc)
