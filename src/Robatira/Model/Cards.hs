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

data PlayerType = Human | Computer deriving (Show)

data Player = Player { playerType :: PlayerType
                     , name :: String
                     , hand :: [Card] } deriving (Show)

data Game = Game { dealingStack :: [Card]
                 , throwingStack :: [Card]
                 , currentPlayer :: Player
                 , otherPlayers :: [Player] } deriving (Show)


-- Initialize with a shuffled deck for a proper game
initGame :: [Card] -> Game
initGame startDeck = Game startStack throwingStack player1 [player2]
  where
   (hand1, deckAfterHand1) = splitAt 7 startDeck
   (hand2, deckAfterHand2) = splitAt 7 deckAfterHand1
   (singleCardStack, deckAfterthrowingStack) = splitAt 1 deckAfterHand2
   throwingStack = singleCardStack
   startStack = deckAfterthrowingStack
   player1 = Player Human "Alice" hand1
   player2 = Player Human "Bob" hand2

