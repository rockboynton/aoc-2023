{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, digitToInt)
import Data.Maybe

firstDigit :: BC.ByteString -> Maybe Int
firstDigit = do
  c <- BC.find isDigit
  pure $ digitToInt <$> c

lastDigit :: BC.ByteString -> Maybe Int
lastDigit = firstDigit . BC.reverse

calVal :: BC.ByteString -> Maybe Int
calVal s = do
  f <- firstDigit s
  l <- lastDigit s
  pure $ f * 10 + l

part1 :: IO ()
part1 = do
  contents <- BC.readFile "input/day1/input1.txt"
  let scrambledCalVals = BC.lines contents
  let calVals = calVal <$> scrambledCalVals
  print $ (sum . map (fromMaybe 0)) calVals

threeChars :: [BC.ByteString]
threeChars = ["one", "two", "six"]

fourChars :: [BC.ByteString]
fourChars = ["four", "five", "nine"]

fiveChars :: [BC.ByteString]
fiveChars = ["three", "seven", "eight"]

bsToDigit :: BC.ByteString -> Int
bsToDigit = \case
  "one" -> 1
  "two" -> 2
  "three" -> 3
  "four" -> 4
  "five" -> 5
  "six" -> 6
  "seven" -> 7
  "eight" -> 8
  "nine" -> 9
  _ -> undefined

firstDigit2 :: BC.ByteString -> Maybe Int
firstDigit2 = go
  where
    go :: BC.ByteString -> Maybe Int
    go xs
      | isDigit $ BC.head xs = pure . digitToInt $ BC.head xs
      | BC.take 3 xs `elem` threeChars = pure . bsToDigit . BC.take 3 $ xs
      | BC.take 4 xs `elem` fourChars = pure . bsToDigit . BC.take 4 $ xs
      | BC.take 5 xs `elem` fiveChars = pure . bsToDigit . BC.take 5 $ xs
      | otherwise = go $ BC.tail xs

lastDigit2 :: BC.ByteString -> Maybe Int
lastDigit2 bs = go $ BC.reverse bs
  where
    go :: BC.ByteString -> Maybe Int
    go xs
      | isDigit $ BC.head xs = pure . digitToInt $ BC.head xs
      | (BC.reverse . BC.take 3) xs `elem` threeChars = pure . bsToDigit $ (BC.reverse . BC.take 3) xs
      | (BC.reverse . BC.take 4) xs `elem` fourChars = pure . bsToDigit $ (BC.reverse . BC.take 4) xs
      | (BC.reverse . BC.take 5) xs `elem` fiveChars = pure . bsToDigit $ (BC.reverse . BC.take 5) xs
      | otherwise = go $ BC.tail xs

calVal2 :: BC.ByteString -> Maybe Int
calVal2 s = do
  f <- firstDigit2 s
  l <- lastDigit2 s
  pure $ f * 10 + l

part2 :: IO ()
part2 = do
  contents <- BC.readFile "input/day1/input1.txt"
  let scrambledCalVals = BC.lines contents
  let calVals = calVal2 <$> scrambledCalVals
  print $ (sum . map (fromMaybe 0)) calVals

main :: IO ()
main = do
  part1
  part2
