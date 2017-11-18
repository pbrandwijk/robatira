module Robatira.Game.Engine

where

import Robatira.Model.Cards
import Robatira.Model.Game
import Robatira.Model.Actions
import Robatira.Model.Exceptions
import Robatira.Game.InputParser
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
  putStrLn $ show game
  action <- getPlayerAction (currentPlayer game)
  let newGameState = performPlayerAction action game
  case newGameState of
    Left (EmptyDealingStackException msg game) -> do
      putStrLn "Dealing stack went empty, refilling"
      gameNewDealingStack <- throwingStackToDealingStack game
      case gameNewDealingStack of
        Left e -> putStrLn (show e)
        Right game -> play $ nextPlayer game
    Left (PlayerResignsException msg game) -> do
      putStrLn ("Taking current player out of game, adding remaining cards " ++
        "to game and reshuffling stack")
      gameCurrentPlayerRemoved <- removeCurrentPlayer game
      play $ gameCurrentPlayerRemoved
    Left e -> putStrLn (show e)
    Right game -> do
      play $ nextPlayer game

getPlayerAction :: Player -> IO Action
getPlayerAction (Player Human _ _) = do
  command <- getLine
  return $ commandToAction command

performPlayerAction :: Action -> Game -> Either (Exception Game) Game
performPlayerAction (Throw card) game = throwCard card game
performPlayerAction (Take) game = takeCard game
performPlayerAction (Resign) game = resign game
