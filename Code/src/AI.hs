module AI where

import Board
import System.Random (randomRIO)
import Control.Monad
import Debug.Trace
import Data.Maybe



data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.

trace' arg = traceShow arg arg

buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove = undefined

-- Update the world state after some time has passed
updateGameState :: GameState -- ^ current game state
                   -> GameState -- ^ new game state after computer move
updateGameState w = w

{- Hint: 'updateGameState' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateGameState should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random valid move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateGameState' should also check if either
 player has won and display a message if so.
-}


evaluateMoves :: Col -> Board -> [Position] -> Position -> Int -> Position
evaluateMoves colour board [] highestMove highestEvaluate = highestMove
evaluateMoves colour board (q:qs) highestMove highestEvaluate = do case makeMove board colour q of
                                                                        Nothing -> (0,0) --Should never be reached, used in safe manner
                                                                        Just board' -> do let potentialMax = evaluate board' colour
                                                                                          if potentialMax <= highestEvaluate
                                                                                                then evaluateMoves colour board qs highestMove highestEvaluate
                                                                                                else evaluateMoves colour board qs q potentialMax

getPossible :: Col -> Board -> [Position]
getPossible colour board = filter (\xpos -> isValid board xpos colour) [(xpos,ypos) | xpos <- [0..(size board -1)], ypos <- [0..(size board - 1)]]

isValid :: Board -> Position -> Col -> Bool
isValid board (xpos, ypos) colour = if (checkPosition board (xpos, ypos)) == False
                                            then do let listWouldBeFlipped = (postiionFlipsList board (xpos, ypos) colour)
                                                    if (length listWouldBeFlipped) /= 0
                                                      then True
                                                      else False
                                            else False

getBestMoveOneDepth :: Board -> Col -> Position
getBestMoveOneDepth board colour = do let listOfPositions = getPossible colour board
                                      evaluateMoves colour board listOfPositions (0,0) (-1)

getRandomMove :: Board -> Col -> IO Position
getRandomMove board colour = do let listOfPositions = getPossible colour board
                                chooseRandom listOfPositions


chooseRandom :: [a]   -- ^ List to get random value from
             -> IO a  -- ^ Returns random value from list
chooseRandom xs = do print (length xs)
                     (xs !!) <$> randomRIO (0, length xs - 1)


getBestReversiInitialMove :: Board -> Col -> Position
getBestReversiInitialMove board colour = do let midpoint = div (size board) 2
                                            let numPieces = length (pieces board)
                                            if numPieces == 1 then
                                                if checkPosition board (midpoint-1, midpoint-1) then
                                                    (midpoint, midpoint)
                                                else if checkPosition board (midpoint, midpoint-1) then
                                                    (midpoint-1, midpoint)
                                                else if checkPosition board (midpoint-1, midpoint) then
                                                    (midpoint, midpoint-1)
                                                else
                                                    (midpoint-1, midpoint-1)
                                             else if numPieces == 2 then
                                                 if checkPosition board (midpoint-1, midpoint-1) then
                                                     if getColour board (midpoint-1, midpoint-1) == colour then
                                                         if not (checkPosition board (midpoint, midpoint)) then
                                                             (midpoint, midpoint)
                                                         else
                                                             (midpoint, midpoint-1)
                                                     else
                                                         if not (checkPosition board (midpoint-1, midpoint)) then
                                                             (midpoint-1, midpoint)
                                                         else
                                                             (midpoint, midpoint-1)
                                                 else if checkPosition board (midpoint, midpoint-1) then
                                                     if getColour board (midpoint, midpoint-1) == colour then
                                                         if not (checkPosition board (midpoint-1, midpoint)) then
                                                             (midpoint-1, midpoint)
                                                         else
                                                             (midpoint-1, midpoint-1)
                                                     else
                                                         if not (checkPosition board (midpoint, midpoint)) then
                                                             (midpoint, midpoint)
                                                         else
                                                             (midpoint-1, midpoint-1)
                                                 else
                                                     if getColour board (midpoint, midpoint) == colour then
                                                         (midpoint-1, midpoint-1)
                                                     else
                                                         (midpoint, midpoint-1)
                                              else
                                                 if not (checkPosition board (midpoint-1, midpoint-1)) then
                                                     (midpoint-1, midpoint-1)
                                                 else if not (checkPosition board (midpoint, midpoint-1)) then
                                                     (midpoint, midpoint-1)
                                                 else if not (checkPosition board (midpoint-1, midpoint)) then
                                                     (midpoint-1, midpoint)
                                                 else
                                                     (midpoint, midpoint)
