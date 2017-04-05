module Robatira.Model.Game

where

import Robatira.Model.Cards


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
