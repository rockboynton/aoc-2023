{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Char
import qualified Data.Vector as V
import Data.Maybe (mapMaybe)

data Schematic
  = Schematic {
      grid :: V.Vector (V.Vector Char)
    , rows :: Int
    , cols :: Int
  }
  deriving (Show)

type Pos
  = (Int, Int)

schematicFromStr :: String -> Schematic
schematicFromStr s = Schematic (V.fromList (map V.fromList ls)) n m
  where
    ls = lines s
    n = length ls
    m = length $ head ls

data SchematicNumber
  = SchematicNumber
  { value :: Int
  , row :: Int
  , startCol :: Int
  , endCol :: Int
  }
  deriving (Show)

isPartNumber :: Schematic -> SchematicNumber -> Bool
isPartNumber schem sn = any (posIsAdjacentToSymbol schem) positionsOccupied
  where
    positionsOccupied = [(sn.row, y) | y <- [sn.startCol..sn.endCol]]


schemNumbers :: Schematic -> [SchematicNumber]
schemNumbers schem = concatMap extractNumbersFromRow (zip [0..] (map V.toList (V.toList schem.grid)))

extractNumbersFromRow :: (Int, [Char]) -> [SchematicNumber]
extractNumbersFromRow (r, chars) = go chars 0 []
  where
    go [] _ acc = acc
    go s@(_:xs) i acc
      | i >= length chars = acc
      | otherwise =
        if null charStr
          then go xs (i + 1) acc
          else go rest (i + length charStr) $ SchematicNumber (read charStr) r i (i + length charStr - 1) : acc
      where
        (charStr, rest) = span isDigit s


posIsAdjacentToSymbol :: Schematic -> Pos -> Bool
posIsAdjacentToSymbol schem pos@(x, y) = isDigit cell && any isEngineSymbol neighbors
  where
    cell :: Char
    cell = schem.grid V.! x V.! y
    isEngineSymbol :: Char -> Bool
    isEngineSymbol c = not (isDigit c) && c /= '.'
    neighbors :: [Char]
    neighbors = getValidNeighborValues schem pos

getNeighbors :: Pos -> [Pos]
getNeighbors (x, y) = [(x+dx, y+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]

validPos :: Schematic -> Pos -> Bool
validPos schem (x, y) = x >= 0 && y >= 0 && x < schem.rows && y < schem.cols

validNeighbors :: Schematic -> Pos -> [Pos]
validNeighbors schem p = filter (validPos schem) (getNeighbors p)

getValidNeighborValues :: Schematic -> Pos -> [Char]
getValidNeighborValues schem pos =
    [schem.grid V.! x V.! y | (x, y) <- validNeighbors schem pos]

gearRatio :: [SchematicNumber] -> Pos -> Maybe Int
gearRatio numbers pos = if length adjacentNumbers /= 2 then Nothing else Just (product (map (.value) adjacentNumbers))
  where
    adjacentNumbers :: [SchematicNumber]
    adjacentNumbers = filter (adjacent pos) numbers
    adjacent :: Pos -> SchematicNumber -> Bool
    adjacent (x, y) n =
      n.row >= y - 1 &&
      n.row <= y + 1 &&
      x >= n.startCol - 1 &&
      x <= n.endCol + 1

parseSymbols :: Schematic -> [Pos]
parseSymbols input = concatMap parseSymbolsInLine indexedLines where
    lines = V.toList input.grid
    indexedLines = zip [0..] lines

parseSymbolsInLine :: (Int, V.Vector Char) -> [Pos]
parseSymbolsInLine (y, line) = mapMaybe maybeStar indexedChars where
    chars = V.toList line
    indexedChars = zip [0..] chars
    maybeStar (x, c) = if c == '*' then Just (x, y) else Nothing

main :: IO ()
main = do
  contents <- readFile "input/day3/input.txt"
  let schematic = schematicFromStr contents
  let numbers = schemNumbers schematic
  let filtered = filter (isPartNumber schematic) numbers
  let nums = map (.value) filtered
  print $ sum nums

  let stars = parseSymbols schematic
  let solution = sum (mapMaybe (gearRatio numbers) stars)
  print solution
