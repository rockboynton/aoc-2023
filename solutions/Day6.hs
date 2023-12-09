{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List ( findIndices )

data Race = Race
  { time :: Int
  , distance :: Int
  }
  deriving Show

mkRace :: Int -> Int -> Race
mkRace t d = Race
  { time = t
  , distance = d
  }

waysToBeat :: Race -> Int
waysToBeat r = length $ findIndices (raceWins r) [0..r.distance + 1]

raceWins :: Race -> Int -> Bool
raceWins r timeHeld = do
  let speed = timeHeld
  let remainingTime = r.time - timeHeld
  let distanceTraveled = speed * remainingTime
  distanceTraveled > r.distance

main :: IO ()
main = do
  contents <- T.readFile "input/day6/input.txt"
  let [times', distances'] = map (read . T.unpack) . T.words . flip (!!) 1 . T.split (==':') <$> T.lines contents

  let races = zipWith mkRace times' distances'
  print races
  print . product . map waysToBeat $ races
