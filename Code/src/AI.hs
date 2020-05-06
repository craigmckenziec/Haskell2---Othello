{-|
Module : AI
Description : Module that handles the AI operations of the program, including finding the best move based upon the current board state.
-}
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

-- | Used during bugfixing in order to check the value of variables or function returns
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


-- |Loops through each given possible move to find what move is regarded as the "best" according the the one depth AI and then returns said move after the best move has been found by reaching the end of the list.
evaluateMoves :: Col -- ^ The colour of the current AI
                -> Board -- ^ Current board state
                -> [Position] -- ^ The list of valid moves the AI could make
                -> Position -- ^ The current best move
                -> Int -- ^ The evaluation score of the current best move
                -> Position
evaluateMoves colour board [] highestMove highestEvaluate = highestMove
evaluateMoves colour board (q:qs) highestMove highestEvaluate = do case makeMove board colour q of
                                                                        Nothing -> (0,0) --Should never be reached, used in safe manner
                                                                        Just board' -> do let potentialMax = evaluateBoard board' colour
                                                                                          if potentialMax <= highestEvaluate
                                                                                                then evaluateMoves colour board qs highestMove highestEvaluate
                                                                                                else evaluateMoves colour board qs q potentialMax

-- |Finds all possible moves that the current player could find
getPossible :: Col -- ^ The colour of the current player
            -> Board -- ^ Current board state
            -> [Position] -- ^ List of possible moves
getPossible colour board = filter (\xpos -> isValid board xpos colour) [(xpos,ypos) | xpos <- [0..(size board -1)], ypos <- [0..(size board - 1)]]


-- | Checks if a given move is valid or not for the current player (is on an empty place and successfully flips a piece)
isValid :: Board -- ^ Current board state
        -> Position -- ^ Move to be investigated
        -> Col -- ^ Colour of the current player
        -> Bool -- ^ Bool represnting whether or not the move is valid 
isValid board (xpos, ypos) colour = if (checkPosition board (xpos, ypos)) == False
                                            then do let listWouldBeFlipped = (positionFlipsList board (xpos, ypos) colour)
                                                    if (length listWouldBeFlipped) /= 0
                                                      then True
                                                      else False
                                            else False

-- | Returns the best move according to the one depth AI
getBestMoveOneDepth :: Board -- ^ Current board state
                    -> Col -- ^ Current player colour
                    -> Position -- ^ What the AI believes to be the best move
getBestMoveOneDepth board colour = do let listOfPositions = getPossible colour board
                                      evaluateMoves colour board listOfPositions (0,0) (-1)


-- | Returns a random move
getRandomMove :: Board -- ^ Current board state
                -> Col -- ^ Current player colour
                -> IO Position -- ^ Random move (must be IO due to random usage but is cleared in method one level higher)
getRandomMove board colour = do let listOfPositions = getPossible colour board
                                chooseRandom listOfPositions


-- | Takes a list of elements and returns one at random
chooseRandom :: [a]   -- ^ List to get random value from
             -> IO a  -- ^ Returns random value from list
chooseRandom xs = do (xs !!) <$> randomRIO (0, length xs - 1)

-- | Gets the best initial move for reversi placement (according to the AI)
getBestReversiInitialMove :: Board -- ^ Current board state
                            -> Col -- ^ Current player colour
                            -> Position -- ^ Best initial placement 
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
                                                         if not (checkPosition board (midpoint-1, midpoint)) then
                                                             (midpoint-1, midpoint)
                                                         else
                                                             (midpoint, midpoint-1)
                                                     else
                                                         if not (checkPosition board (midpoint, midpoint)) then
                                                             (midpoint, midpoint)
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
