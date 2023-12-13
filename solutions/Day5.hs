{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Map qualified as M
import Data.List.Split
import Data.Ix

data Category
  = Seed
  | Soil
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location
  deriving(Ord, Eq, Show)

data Almanac
  = Almanac
  { seeds :: [Int]
  , mappings :: M.Map (Category, Category) CategoryMap
  }
  deriving Show

newtype CategoryMap
  = CategoryMap [Range]
  deriving Show

data Range
  = Range
  { srcStart :: Int
  , destStart :: Int
  , len :: Int
  }
  deriving Show

data SeedEncoding
  = Individual
  | Pairs

parseInput :: [T.Text] -> SeedEncoding -> Almanac
parseInput (x:xs) enc = do
  let seedNums = read . T.unpack <$> drop 1 (T.words x)
  let seeds = case enc of
        Individual -> seedNums
        Pairs -> allSeeds seedNums
  let ys = splitOn [""] (drop 1 xs)
  let pairs = map mkCategoryMap ys

  -- let mappings = for (splitOn [T.pack ""] (drop 1 xs)) \(y:ys) -> do
  --   let key = strToKey y
  --   let ranges = map mkRange ys

  --   (key, CategoryMap ranges)
  let mappings = M.fromList pairs
  Almanac{..}
parseInput [] _ = error "bad input"

allSeeds :: [Int] -> [Int]
allSeeds xs = concatMap toRange (chunks xs)

chunks :: [Int] -> [(Int, Int)]
chunks []         = []
chunks (x:y:rest) = (x, y) : chunks rest
chunks _          = error "List length must be even"

toRange :: (Int, Int) -> [Int]
toRange (start, len) = [start..start + len]

strToKey :: T.Text -> (Category, Category)
strToKey "humidity-to-location map:" = (Humidity, Location)
strToKey "temperature-to-humidity map:" = (Temperature, Humidity)
strToKey "light-to-temperature map:" = (Light, Temperature)
strToKey "water-to-light map:" = (Water, Light)
strToKey "fertilizer-to-water map:" = (Fertilizer, Water)
strToKey "soil-to-fertilizer map:" = (Soil, Fertilizer)
strToKey "seed-to-soil map:" = (Seed, Soil)
strToKey _ = error "bad input"

mkRange :: [T.Text] -> Range
mkRange [ds, ss, l] = do
  let srcStart = read . T.unpack $ ss
  let destStart = read . T.unpack $ ds
  let len = read . T.unpack $ l
  Range{..}
mkRange _ = error "bad input"

mkCategoryMap :: [T.Text] -> ((Category, Category), CategoryMap)
mkCategoryMap (x:xs) = do
  let key = strToKey x
  let ranges = map (mkRange . T.words) xs
  (key, CategoryMap ranges)
mkCategoryMap _ = error "bad input"

main :: IO ()
main = do
  contents <- T.readFile "input/day5/input.txt"

  let almanac = parseInput (T.lines contents) Individual
  print . minLoc $ almanac

  let almanac' = parseInput (T.lines contents) Pairs
  print . minLoc $ almanac'

getMapping :: M.Map (Category, Category) CategoryMap -> (Category, Category) -> Int -> Int
getMapping m k v = do
  let (CategoryMap ranges) = m M.! k
  go ranges
  where
    go :: [Range] -> Int
    go [] = v
    go (x:xs) =
      if inRange (x.srcStart, x.srcStart + x.len) v
        then v + (x.destStart - x.srcStart)
        else go xs

minLoc :: Almanac -> Int
minLoc almanac =
  minimum
    . map ( gm (Humidity, Location)
          . gm (Temperature, Humidity)
          . gm (Light, Temperature)
          . gm (Water, Light)
          . gm (Fertilizer, Water)
          . gm (Soil, Fertilizer)
          . gm (Seed, Soil))
    $ almanac.seeds
  where
    gm = getMapping almanac.mappings
