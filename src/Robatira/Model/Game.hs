module Robatira.Model.Game

where

import Robatira.Model.Cards
import Robatira.Model.Actions
import Robatira.Model.Exceptions
import Robatira.Util.Shuffle

import Data.List (delete)
import Data.Either

data PlayerType = Human | Computer deriving (Show)

data Player = Player { playerType :: PlayerType
                     , name :: String
                     , hand :: [Card] } deriving (Show)

data Game = Game { dealingStack :: [Card]
                 , throwingStack :: [Card]
                 , currentPlayer :: Player
                 , otherPlayers :: [Player] } deriving (Show)


-- Initialize with a stack of cards and a list of players
-- Note: As a rule of thumb, for a proper game the stack of cards should be at
--       least three times as much as number of cards needed to deal to the 
--       players.
--       Also note that for a proper game the stack of cards should consist of
--       of one or more full decks and should be well shuffled.
initGame :: [Card] -> [Player] -> Either Exception Game
initGame startDeck players
    | hasNotEnoughCards = Left (NotEnoughCardsException ("Not enough cards "
                ++ "for a proper game!"))
    | otherwise = Right (Game startStack throwingStack firstPlayer
                            otherPlayers)
  where
   hasNotEnoughCards = length players * handSizeAtStart * 3 > length startDeck
   (dealtPlayers,cardsAfterDealing) = dealCardsToEachPlayer players [] startDeck
   throwingStack = [head cardsAfterDealing]
   startStack = tail cardsAfterDealing
   firstPlayer = head dealtPlayers
   otherPlayers = tail dealtPlayers

-- Number of cards that each player is dealt at the start of the game
handSizeAtStart = 7

-- Helper function to deal the cards to the players
dealCardsToEachPlayer :: [Player] -> [Player] -> [Card] -> ([Player],[Card])
dealCardsToEachPlayer [] ds cards = (ds, cards)
dealCardsToEachPlayer (p:ps) ds cards = 
  dealCardsToEachPlayer ps (dp:ds) newCards
    where
      (newHand, newCards) = splitAt handSizeAtStart cards
      dp = p { hand = newHand }

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
throwCard :: Card -> Game -> Either Exception Game
throwCard card game | elem card playerHand = Right game { 
                                    throwingStack = (card:gameTs),
                                    currentPlayer = player { hand = newHand } }
                    | otherwise = Left (ThrowingCardNotInHandException
            ("Cannot throw card " ++ show card ++ "when not in hand!"))
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
