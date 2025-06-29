{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

-- https://open.kattis.com/problems/vote
import Control.Category ((>>>))
import Control.Monad (replicateM)
import Control.Monad.State (MonadState (get, put), State, evalState)

type Election = [Int]

main :: IO ()
main =
  interact $
    runScanner elections >>> map solve >>> unlines

runScanner :: Scanner a -> String -> a
runScanner = runScannerWith lines

elections :: Scanner [Election]
elections = numberOf (numberOf int)

solve :: Election -> String
solve votes =
  let total = sum votes
      candidates = zip [1 ..] votes
      (winner, maxVotes, hasTie) = findWinner candidates
   in case hasTie of
        True -> "no winner"
        False -> formatWinner winner maxVotes total

findWinner :: [(Int, Int)] -> (Int, Int, Bool)
findWinner candidates =
  let maxVotes = maximum (map snd candidates)
      winners = filter ((== maxVotes) . snd) candidates
   in case winners of
        [(i, v)] -> (i, v, False)
        _ -> (0, maxVotes, True)

formatWinner :: Int -> Int -> Int -> String
formatWinner candidate votes total
  | votes > total `div` 2 = "majority winner " ++ show candidate
  | otherwise = "minority winner " ++ show candidate

-- Scanner
type Scanner = State [String]

runScannerWith :: (String -> [String]) -> Scanner a -> String -> a
runScannerWith f s = evalState s . f

-- | gets the current list of tokens, puts it back without the first token, and returns the first token
str :: Scanner String
str = get >>= \case s : ss -> put ss >> return s

-- | read next token as 'Int'
int :: Scanner Int
int = read <$> str

-- | parse next token as `Int` and then run the provided scanner that many times returning list of results
numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s
