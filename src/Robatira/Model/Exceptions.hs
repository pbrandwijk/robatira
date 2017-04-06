module Robatira.Model.Exceptions

where

data Exception = NotEnoughCardsException String 
               | ThrowingCardNotInHandException String
               | EmptyDealingStackException String
                 deriving (Show, Eq)
