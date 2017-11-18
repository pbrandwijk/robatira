module Robatira.AI.AI

where



-- Stateless

-- In the stateless AI the computer recieves the following information of the 
-- current game state
-- His own hand
-- Number of cards for each other player in the game, and the subsequent order
--   of next players to come
-- All cards on the throwing stack


-- Stateful

-- In stateful AI a data type must be defined in which the information visible
-- to the AI module for each game state is stored. The AI module can use this
-- historic data of the game to make a better informed next move.
