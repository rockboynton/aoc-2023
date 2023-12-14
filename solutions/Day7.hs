{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List ( sort, group )

newtype Game
  = Game
  { handBidPairs :: [HandBidPair]
  }
  deriving Show

newtype HandBidPair
  = HandBidPair (Hand, Bid)
  deriving Show

instance Eq HandBidPair where
  (==) :: HandBidPair -> HandBidPair -> Bool
  (HandBidPair l) == (HandBidPair r) = fst l == fst r

instance Ord HandBidPair where
  compare :: HandBidPair -> HandBidPair -> Ordering
  compare (HandBidPair l) (HandBidPair r) = compare (fst l) (fst r)


mkGame :: [T.Text] -> Game
mkGame = Game . map (mkHandBidPair . T.words)
  where
    mkHandBidPair :: [T.Text] -> HandBidPair
    mkHandBidPair [handStr, bidStr] = HandBidPair (mkHand (T.unpack handStr), Bid $ (read . T.unpack) bidStr)
    mkHandBidPair _ = error "bad input"

newtype Bid
  = Bid Int
  deriving Show

data Hand
  = Hand
  { handType :: HandType
  , cards :: [Card]
  }
  deriving (Show, Eq, Ord)

mkHand :: [Char] -> Hand
mkHand handStr = do
  let cards = map mkCard handStr
  let handType = getHandType cards
  Hand{..}

getHandType :: [Card] -> HandType
getHandType cs
  | hasFiveOfAKind cs = FiveOfAKind
  | hasFourOfAKind cs = FourOfAKind
  | isFullHouse cs = FullHouse
  | hasThreeOfAKind cs = ThreeOfAKind
  | hasTwoPairs cs = TwoPair
  | hasPair cs = OnePair
  | otherwise = HighCard

isFullHouse :: [Card] -> Bool
isFullHouse hand = hasPair hand && hasThreeOfAKind hand

hasPair :: [Card] -> Bool
hasPair hand = 2 `elem` map length (group $ sort hand)

hasTwoPairs :: [Card] -> Bool
hasTwoPairs hand = length (filter (==2) (map length (group $ sort hand))) == 2

hasThreeOfAKind :: [Card] -> Bool
hasThreeOfAKind hand = 3 `elem` map length (group $ sort hand)

hasFourOfAKind :: [Card] -> Bool
hasFourOfAKind hand = 4 `elem` map length (group $ sort hand)

hasFiveOfAKind :: [Card] -> Bool
hasFiveOfAKind hand = 5 `elem` map length (group $ sort hand)

data Card
  = C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | T
  | J
  | Q
  | K
  | A
  deriving (Show, Eq, Ord)

mkCard :: Char -> Card
mkCard = \case
  '2' -> C2
  '3' -> C3
  '4' -> C4
  '5' -> C5
  '6' -> C6
  '7' -> C7
  '8' -> C8
  '9' -> C9
  'T' -> T
  'J' -> J
  'Q' -> Q
  'K' -> K
  'A' -> A
  _ -> error "bad input"

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

newtype Rank
  = Rank Int
  deriving Show

score :: (HandBidPair, Rank) -> Int
score (HandBidPair (_, Bid b), Rank r) = b * r

main :: IO ()
main = do
  contents <- T.readFile "input/day7/input.txt"
  let game = mkGame $ T.lines contents
  let sortedHandBidPairs = sort game.handBidPairs
  let withRanks = zip sortedHandBidPairs $ Rank <$> [1..]
  let winnings = map score withRanks
  let totalWinnings = sum winnings
  print totalWinnings
