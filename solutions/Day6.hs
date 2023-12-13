{-# LANGUAGE OverloadedRecordDot #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T

data Race = Race
  { time :: Int
  , distance :: Int
  }
  deriving Show

waysToBeat :: Race -> Int
waysToBeat r = length $ filter (raceWins r) [0..r.time]

raceWins :: Race -> Int -> Bool
raceWins r timeHeld = distanceTraveled > r.distance
  where
    speed = timeHeld
    remainingTime = r.time - timeHeld
    distanceTraveled = speed * remainingTime

main :: IO ()
main = do
  contents <- T.readFile "input/day6/input.txt"
  let [times', distances'] = map (read . T.unpack) . T.words . flip (!!) 1 . T.split (==':') <$> T.lines contents
  let races = zipWith Race times' distances'
  print . product . map waysToBeat $ races

  let [time, distance] = read . T.unpack . T.concat . T.words . flip (!!) 1 . T.split (==':') <$> T.lines contents
  let race = Race time distance
  print $ waysToBeat race
