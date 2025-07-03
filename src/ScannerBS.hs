-- https://github.com/byorgey/comprog-hs/blob/master/ScannerBS.hs
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ScannerBS where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState (get, put), State, evalState)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromJust, listToMaybe)

type Scanner = State [C.ByteString]

runScanner :: Scanner a -> C.ByteString -> a
runScanner = runScannerWith C.words

runScannerWith :: (C.ByteString -> [C.ByteString]) -> Scanner a -> C.ByteString -> a
runScannerWith f s = evalState s . f

str :: Scanner C.ByteString
str = get >>= \case s : ss -> put ss >> return s

int :: Scanner Int
int = (fst . fromJust . C.readInt) <$> str

integer :: Scanner Integer
integer = (read . C.unpack) <$> str

double :: Scanner Double
double = (read . C.unpack) <$> str

decimal :: Int -> Scanner Int
decimal p = (round . ((10 ^ p) *)) <$> double

-- | parse next token as `Int` and then run the provided scanner that many times returning list of results
numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

peek :: Scanner C.ByteString
peek = (fromJust . listToMaybe) <$> get

till :: (C.ByteString -> Bool) -> Scanner a -> Scanner [a]
till p s = do
  t <- peek
  case p t of
    True -> return []
    False -> (:) <$> s <*> till p s

pair :: Scanner a -> Scanner b -> Scanner (a, b)
pair = liftA2 (,)
