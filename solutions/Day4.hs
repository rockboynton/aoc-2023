{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Search as BS

data ScratchCard
  = ScratchCard
  { winningNumbers :: [Int]
  , heldNumbers :: [Int]
  }
  deriving (Show)

mkScratchCard :: B.ByteString -> ScratchCard
mkScratchCard bs = do
  let [_, allNums] = BS.split ": " bs
  let [winningNumbersStr, heldNumbersStr] = BS.split " | " allNums
  let winningNumbers = map (read . B.unpack) (B.words winningNumbersStr)
  let heldNumbers = map (read . B.unpack) (B.words heldNumbersStr)
  ScratchCard {..}

scratchCardScore :: ScratchCard -> Int
scratchCardScore sc = if (not . null) winningNumbersHeld then 2 ^ (length winningNumbersHeld - 1) else 0
  where
    winningNumbersHeld = filter (`elem` sc.heldNumbers) sc.winningNumbers

main :: IO ()
main = do
  contents <- B.readFile "input/day4/input.txt"
  let ls = B.lines contents
  let cards = map mkScratchCard ls
  let scores = map scratchCardScore cards
  print $ sum scores
