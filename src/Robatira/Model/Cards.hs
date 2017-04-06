module Robatira.Model.Cards 

where

import Data.Char (chr)


data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq, Enum)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | 
              Ten | Jack | Queen | King deriving (Show, Eq, Enum)

data Joker = RedJoker | BlackJoker | WhiteJoker deriving (Show, Eq, Enum)

data Card = Regular Suit Rank | Other Joker deriving (Eq)

instance Show Card where
  show card = [chr $ cardToUnicode card]

-- Convert a playing card to its Unicode representation
cardToUnicode :: Card -> Int
cardToUnicode (Regular Spades rank) = 0x1F0A0 + (rankToHex rank)
cardToUnicode (Regular Hearts rank) = 0x1F0B0 + (rankToHex rank)
cardToUnicode (Regular Diamonds rank) = 0x1F0C0 + (rankToHex rank)
cardToUnicode (Regular Clubs rank) = 0x1F0D0 + (rankToHex rank)
cardToUnicode (Other RedJoker) = 0x1F0BF
cardToUnicode (Other BlackJoker) = 0x1F0CF
cardToUnicode (Other WhiteJoker) = 0x1F0DF

-- Need to treat King and Queen explicitly as Unicode defines an extra 
-- rank "C" at 0xC
rankToHex :: Rank -> Int
rankToHex King = 0xE
rankToHex Queen = 0xD
rankToHex rank = 0xE - (length (enumFrom rank))

-- Useful for representing the cards of other players, which should be 
-- countable, but not open
backOfCardUnicode :: Int
backOfCardUnicode = 0x1F0A0


allSuits = [Spades ..]
allRanks = [Ace ..]
allJokers = [RedJoker ..]

frenchStandard52Plus3Jokers :: [Card]
frenchStandard52Plus3Jokers = [Regular c f | c <- allSuits, f <- allRanks] 
            ++ [Other j | j <- allJokers]

