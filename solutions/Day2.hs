{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Search as BS
import Data.ByteString (ByteString)
import qualified Debug.Trace as Debug

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
  s -> error $ "could not create color from " ++ BC.unpack s

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
  deriving (Show)

parseHandful :: ByteString -> Handful
parseHandful bs = do
  let countsWithColors = BS.split ", " bs
  Handful $ M.fromList (map parseCube countsWithColors)

validHandful :: Handful -> Bool
validHandful handful = all (validHandfulForColor handful) colors

newtype Game
  = Game (Int, [Handful])
  deriving (Show)

parseGame :: BC.ByteString -> Game
parseGame bs = do
  let (idxStr : [setStr]) = BS.split ": " bs
  let (_: [idx]) = BC.split ' ' idxStr
  let Just (gameIdx, _) = B.readInt idx

  let handfulls = BS.split "; " setStr
  let handfulls' = map parseHandful handfulls

  Game (gameIdx, handfulls')

possibleGame :: Game -> Bool
possibleGame (Game (_, handfulls)) = all validHandful handfulls

gameIdx :: Game -> Int
gameIdx (Game (idx, _)) = idx

part1 :: IO ()
part1 = do
  contents <- BC.readFile "input/day2/input.txt"
  let games = BC.lines contents
  let games' = map parseGame games
  let possibleGames = filter possibleGame games'
  let solution = sum $ map gameIdx possibleGames
  print solution

main :: IO ()
main = do
  part1
