{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Search as BS
import Data.ByteString (ByteString)
import Data.Foldable (foldl')

data Color
  = Red
  | Green
  | Blue
  deriving(Ord, Eq, Show)

colors :: [Color]
colors = [Red, Green, Blue]

maxNumCubes :: Color -> Int
maxNumCubes = \case
  Red -> 12
  Green -> 13
  Blue -> 14

colorFromStr :: BC.ByteString -> Color
colorFromStr = \case
  "red" -> Red
  "green" -> Green
  "blue" -> Blue
  s -> undefined

parseCube :: ByteString -> (Color, Int)
parseCube bs = do
  let Just (count, rest) = BC.readInt bs
  let color = colorFromStr (BC.strip rest)
  (color, count)

validHandfulForColor :: Handful -> Color -> Bool
validHandfulForColor (Handful m) color = case m M.!? color of
  Just num -> num <= maxNumCubes color
  Nothing -> True

newtype Handful
  = Handful (M.Map Color Int)
  deriving (Show, Eq, Ord)

parseHandful :: ByteString -> Handful
parseHandful bs = do
  let countsWithColors = BS.split ", " bs
  Handful $ M.fromList (map parseCube countsWithColors)

validHandful :: Handful -> Bool
validHandful handful = all (validHandfulForColor handful) colors

minHandful :: Ord k => Ord a => [M.Map k a] -> M.Map k a
minHandful = foldl' (M.unionWith max) M.empty

newtype Game
  = Game (Int, [Handful], Handful)
  deriving (Show)

parseGame :: BC.ByteString -> Game
parseGame bs = do
  let (idxStr : [setStr]) = BS.split ": " bs
  let (_: [idx]) = BC.split ' ' idxStr
  let Just (gameIdx, _) = B.readInt idx

  let handfulls = BS.split "; " setStr
  let handfulls' = map parseHandful handfulls
  let handfulls'' = map (\(Handful m) -> m) handfulls'
  let maxes = minHandful handfulls''

  Game (gameIdx, handfulls', Handful maxes)

possibleGame :: Game -> Bool
possibleGame (Game (_, handfulls, _)) = all validHandful handfulls

gameIdx :: Game -> Int
gameIdx (Game (idx, _, _)) = idx

gamePower :: Game -> Int
gamePower (Game (_, _, Handful maxCubes)) = product $ M.elems maxCubes

part1 :: [Game] -> IO ()
part1 games= do
  let possibleGames = filter possibleGame games
  let solution = sum $ map gameIdx possibleGames
  print solution

part2 :: [Game] -> IO ()
part2 games = do
  let solution = sum $ map gamePower games
  print solution

main :: IO ()
main = do
  contents <- BC.readFile "input/day2/input.txt"
  let games = BC.lines contents
  let games' = map parseGame games
  part1 games'
  part2 games'
