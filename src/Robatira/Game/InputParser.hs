module Robatira.Game.InputParser

where

import Robatira.Model.Cards
import Robatira.Model.Actions
import Robatira.Model.Exceptions

import Data.Either

commandToAction :: String -> Action
commandToAction "take" = Take
commandToAction (rank:suit:[]) = Throw (Regular (chrToSuit suit)
  (chrToRank rank))
commandToAction "resign" = Resign
commandToAction _ = Take

chrToSuit :: Char -> Suit
chrToSuit 'S' = Spades
chrToSuit 'H' = Hearts
chrToSuit 'D' = Diamonds
chrToSuit 'C' = Clubs

chrToRank :: Char -> Rank
chrToRank 'A' = Ace
chrToRank 'K' = King
chrToRank 'Q' = Queen
chrToRank 'J' = Jack
chrToRank 'T' = Ten
chrToRank '9' = Nine
chrToRank '8' = Eight
chrToRank '7' = Seven
chrToRank '6' = Six
chrToRank '5' = Five
chrToRank '4' = Four
chrToRank '3' = Three
chrToRank '2' = Two

