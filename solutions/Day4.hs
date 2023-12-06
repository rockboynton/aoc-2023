{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Search as BS

data ScratchCard
  = ScratchCard
  { winningNumbers :: [Int]
  , heldNumbers :: [Int]
  , numCopies :: Int
  }
  deriving (Show)

mkScratchCard :: B.ByteString -> ScratchCard
mkScratchCard bs = do
  let [_, allNums] = BS.split ": " bs
  let [winningNumbersStr, heldNumbersStr] = BS.split " | " allNums
  let winningNumbers = map (read . B.unpack) (B.words winningNumbersStr)
  let heldNumbers = map (read . B.unpack) (B.words heldNumbersStr)
  let numCopies = 1
  ScratchCard {..}

scratchCardScore :: ScratchCard -> Int
scratchCardScore sc = if (not . null) (winningNumbersHeld sc) then 2 ^ (length (winningNumbersHeld sc) - 1) else 0

winningNumbersHeld :: ScratchCard -> [Int]
winningNumbersHeld sc = filter (`elem` sc.heldNumbers) sc.winningNumbers

-- copied from mrkline
moreCards :: [Int] -> [Int] -> [Int]
moreCards (w:ws) (c:cs) = c : moreCards ws cs'
  where
    awarded = replicate w c <> repeat 0
    cs' = zipWith (+) awarded (cs <> repeat 0)
moreCards _ _ = []

main :: IO ()
main = do
  contents <- B.readFile "input/day4/input.txt"
  let ls = B.lines contents
  let cards = mkScratchCard <$> ls
  let scores = scratchCardScore <$> cards
  print $ sum scores

  let wins = length . winningNumbersHeld <$> cards
  print . sum $ moreCards wins (repeat 1)
