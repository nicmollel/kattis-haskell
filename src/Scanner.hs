-- https://github.com/byorgey/comprog-hs/blob/master/Scanner.hs
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Scanner where

import Control.Monad (replicateM)
import Control.Monad.State

type Scanner = State [String]

runScanner :: Scanner a -> String -> a
runScanner = runScannerWith words

runScannerWith :: (String -> [String]) -> Scanner a -> String -> a
runScannerWith f s = evalState s . f

-- | gets the current list of tokens, puts it back without the first token, and returns the first token
str :: Scanner String
str = get >>= \case s : ss -> put ss >> return s

-- | read next token as 'Int'
int :: Scanner Int
int = read <$> str

-- | read next token as 'Integer'
integer :: Scanner Integer
integer = read <$> str

-- | read next token as 'Double'
double :: Scanner Double
double = read <$> str

-- | read next token as `Double` then convert it to `Int` by dropping `p` decimal places
decimal :: Int -> Scanner Int
decimal p = (round . ((10 ^ p) *)) <$> double

-- | parse next token as `Int` and then run the provided scanner that many times returning list of results
numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

-- | repeats the scanner 's' as many times as it can, returning a list of the results
many :: Scanner a -> Scanner [a]
many s = get >>= \case [] -> return []; _ -> (:) <$> s <*> many s

-- | @'times' n s@ runs scanner 's' 'n' times and returns the list of results
times :: Int -> Scanner a -> Scanner [a]
times = replicateM

(><) :: Int -> Scanner a -> Scanner [a]
(><) = times

two, three, four :: Scanner a -> Scanner [a]
[two, three, four] = map times [2 .. 4]
