module Robatira.Game.Engine

where

import Robatira.Model.Cards
import Robatira.Model.Game
import Robatira.Model.Actions
import Robatira.Util.Shuffle (shuffle)

-- Starting the game is setting up the initial game state and starting off the 
-- first player
start :: IO ()
start = do
  shuffledDeck <- shuffle frenchStandard52Plus3Jokers
  let players = [createHumanPlayer "Esther", createHumanPlayer "Pieter"]
  let initializedGame = initGame shuffledDeck players
  case initializedGame of
    Right game -> play game
    Left e -> putStrLn (show e)

createHumanPlayer :: String -> Player
createHumanPlayer name = Player Human name []
  

-- Playing the game means the current player either picks a card from the 
-- dealing stack or throws a legal card on the throwing stack. 
-- If the current player after this has no more cards left, the game is over.
-- Else, the next player on the list becomes the current player and the whole 
-- process reiterates
play :: Game -> IO ()
play game = do
  let action = getPlayerAction (currentPlayer game)
  let newGameState = performPlayerAction action game
  case newGameState of
    (Nothing) -> putStrLn "Player action could not be performed"
    (Just g) -> do
                 putStrLn $ show g
                 play g

getPlayerAction :: Player -> Action
getPlayerAction player = Take

performPlayerAction :: Action -> Game -> Maybe Game
performPlayerAction (Throw card) game = throwCard card game
performPlayerAction (Take) game = Just (takeCard game)
