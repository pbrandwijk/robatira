module Robatira.Model.Cards 

where


data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq, Enum)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | 
              Ten | Jack | Queen | King deriving (Show, Eq, Enum)

data Joker = RedJoker | BlackJoker | WhiteJoker deriving (Show, Eq, Enum)

data Card = Regular Suit Rank | Other Joker deriving (Show, Eq)


allSuits = [Hearts ..]
allRanks = [Two ..]
allJokers = [RedJoker ..]

frenchStandard52Plus3Jokers :: [Card]
frenchStandard52Plus3Jokers = [Regular c f | c <- allSuits, f <- allRanks] 
            ++ [Other j | j <- allJokers]

