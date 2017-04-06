module Robatira.Model.Game

where

import Robatira.Model.Cards
import Robatira.Model.Actions
import Robatira.Util.Shuffle (shuffle)

import Data.List (delete)

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

-- Current player takes top card from dealing stack and adds it to their 
-- own hand 
takeCard :: Game -> Game
takeCard (Game (topcard:cs) ts (Player ptype name hand) ops) = Game {
  dealingStack = cs,
  throwingStack = ts,
  currentPlayer = (Player ptype name (topcard:hand)),
  otherPlayers = ops}

-- Current player throws a card from their hand on to the top of the 
-- throwing stack. Function is wrapped in Maybe monad to handle the case
-- where the throwing card is not part of the player's hand.
throwCard :: Card -> Game -> Maybe Game
throwCard card game | elem card playerHand = Just game { 
                                    throwingStack = (card:gameTs),
                                    currentPlayer = player { hand = newHand } }
                    | otherwise = Nothing
  where 
    playerHand = hand $ currentPlayer game 
    gameTs = throwingStack game
    player = currentPlayer game
    newHand = delete card playerHand
    

-- Current player goes to end of other player list. Head player of other player 
-- list becomes new current player
nextPlayer :: Game -> Game
nextPlayer (Game ds ts cp ops) = Game { 
  dealingStack = ds,
  throwingStack = ts,
  currentPlayer = head ops,
  otherPlayers = (tail ops) ++ [cp] }

-- Take all cards of throwing stack except the top card. Join them with the
-- dealing stack and shuffle the whole dealing stack.
throwingStackToDealingStack :: Game -> IO Game
throwingStackToDealingStack game = do
  let ts = throwingStack game
  let newStack = tail ts
  let newTs = [head ts]
  shuffledStack <- shuffle $ (dealingStack game) ++ newStack
  let newDs = shuffledStack
  return game { dealingStack = newDs, throwingStack = newTs }

-- The game is over when the current player has no cards left after throwing a 
-- card on the throwing stack. In this case the current player is the winner.
isGameOver :: Game -> Bool
isGameOver game = hand (currentPlayer game) == []
