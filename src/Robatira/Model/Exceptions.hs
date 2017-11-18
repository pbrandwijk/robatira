module Robatira.Model.Exceptions

where

type ExceptionMessage = String

data Exception a = NotEnoughCardsException ExceptionMessage 
               | ThrowingCardNotInHandException ExceptionMessage
               | EmptyDealingStackException ExceptionMessage a
               | NoCardsToRefillDealingStack ExceptionMessage
               | PlayerResignsException ExceptionMessage a
                 deriving (Show, Eq)
