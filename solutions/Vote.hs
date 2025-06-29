module Vote where

-- https://open.kattis.com/problems/vote
import Control.Category ((>>>))
import Scanner (Scanner, int, numberOf, runScannerWith)

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
