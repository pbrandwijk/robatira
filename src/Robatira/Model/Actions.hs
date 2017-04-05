module Robatira.Model.Actions

where

import Robatira.Model.Cards

data Action = Throw Card | Take | Resign deriving (Show, Eq)

commandToAction :: String -> Action
commandToAction command = Resign
