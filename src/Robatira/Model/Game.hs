module Robatira.Model.Game

where

import Robatira.Model.Cards
import Robatira.Util.Shuffle (shuffle)


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

-- Starting the game is setting up the initial game state and starting off the 
-- first player
start :: IO ()
start = do
  shuffledDeck <- shuffle frenchStandard52Plus3Jokers
  let initializedGame = initGame shuffledDeck
  play initializedGame
  

-- Playing the game means the current player either picks a card from the 
-- dealing stack or throws a legal card on the throwing stack. 
-- If the current player after this has no more cards left, the game is over.
-- Else, the next player on the list becomes the current player and the whole 
-- process reiterates
play :: Game -> IO ()
play game = do
  putStrLn $ show game

-- Current player takes top card from dealing stack and adds it to their 
-- own hand 
takeCard :: Game -> Game
takeCard (Game (topcard:cs) ts (Player ptype name hand) ops) = Game {
  dealingStack = cs,
  throwingStack = ts,
  currentPlayer = (Player ptype name (topcard:hand)),
  otherPlayers = ops}

-- Current player goes to end of other player list. Head player of other player 
-- list becomes new current player
nextPlayer :: Game -> Game
nextPlayer (Game ds ts cp ops) = Game { 
  dealingStack = ds,
  throwingStack = ts,
  currentPlayer = head ops,
  otherPlayers = (tail ops) ++ [cp] }                                      

-- The game is over when the current player has no cards left after throwing a 
-- card on the throwing stack. In this case the current player is the winner.
isGameOver :: Game -> Bool
isGameOver game = hand (currentPlayer game) == []
