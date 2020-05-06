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


-- | Datatype representing a single leaf of the a game tree
data GameTree = GameTree { game_board :: Board, -- ^ Board resultant from this nodes move
                           game_turn :: Col, -- ^ Who's turn it is at this point in the tree
                           next_moves :: [(Position, GameTree)] } -- ^ List of next moves that could be made at this point aswell as the resultant GameTrees from these moves


-- | Used during bugfixing in order to check the value of variables or function returns
trace' arg = traceShow arg arg

-- | Builds a tree of possible moves and their subsequent possible moves until an end game state is reached (i.e. the game is finished)
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree -- ^ GameTree that is built of possible moves
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

-- | Overarching call that gets the best possible move according to the Advanced AI
getBestMoveAdvanced :: Board 
                    -> Col 
                    -> Position
getBestMoveAdvanced board turn = do let wholeTree = buildTree (getPossible) board turn 
                                    evaluateTree wholeTree

-- | Get the largest score from the list of moves, including a safe case of empty
getMax :: [Int] 
        -> Int
getMax [] = -1000 -- No moves is very bad for the AI player so we make this -1
getMax scores = maximum scores


-- | Get the lowest score from the list of moves, including a safe case of empty
getMin :: [Int]
        -> Int
getMin [] = 1000 -- No moves for other player is very good, so this is 1 more than the board size
getMin scores = minimum scores

-- | Search through the tree to a given depth and find the best move that can be made
evaluateTree :: GameTree -- ^ The GameTree to be investigated 
            -> Position -- ^ The "best" move
evaluateTree (GameTree board turn tree) = do let depth = 5 -- Increasing depth will improve results but dramatically reduce speed of operations
                                             snd(maximum (lookAheadOne (depth-1) tree turn))

-- | Looks ahead one position in the tree and build the list of moves that can be made along with their score according to the AI
lookAheadOne :: Int -- ^ Current depth (0 is max)
            -> [(Position, GameTree)] -- ^ List representing Position-Tree pairs to be investigated
            -> Col  -- ^ Current player
            -> [(Int, Position)] -- ^ Int Position pairs representing the possible moves given scores
lookAheadOne _ [] _ = []
lookAheadOne depth ((current,leafs):qs) turn = (exploreBranch depth leafs turn turn, current):lookAheadOne depth qs turn

-- | Explore a given branch of the tree to find what moves give the best results for the AI and the worst results for the other player 
exploreBranch :: Int -- ^ Current depth
                -> GameTree -- ^ Current Tree/Branch being investigated
                -> Col -- ^ Colour of the AI player
                -> Col -- ^ Colour of the current player
                -> Int -- ^ Score of a given board state
exploreBranch 0 leafs aiPlayer _ = evaluateLeaf (game_board leafs) aiPlayer
exploreBranch depth leafs aiPlayer currentPlayer = if (aiPlayer == currentPlayer)
                                                            then getMin (makeFullBranch depth (next_moves leafs) aiPlayer currentPlayer)
                                                            else getMax (makeFullBranch depth (next_moves leafs) aiPlayer currentPlayer)

-- | Builds a score for a given board state according to the strategies given by http://radagast.se/othello/Help/strategy.html
evaluateLeaf :: Board -- ^ Board state being investigated
            -> Col  -- ^ Colour of the AI
            -> Int -- ^ Score assigned to this board by the AI (how good it is for the AI)
evaluateLeaf  (Board size passes []) aiPlayer = 0 -- Adds/detracts nothing from the score as piece end has been reached
evaluateLeaf  (Board size passes (((x,y), pieceColour):qs)) aiPlayer = 
                        if ((x,y) == (0,0) || (x,y) == (size-1,0) || (x,y) == (0, size-1) || (x,y) == (size-1, size-1)) -- Pieces on a corner are very powerful, so target them for us and avoid them for other player
                                    then if pieceColour == aiPlayer 
                                            then evaluateLeaf (Board size passes qs) aiPlayer + 100
                                            else evaluateLeaf (Board size passes qs) aiPlayer - 100
                        else if (x == 0 || y == 0 || x == size-1 || y == size-1) -- Pieces on sides are also powerful so we once again target them for us and avoid them for the other player
                                    then if pieceColour == aiPlayer
                                            then evaluateLeaf (Board size passes qs) aiPlayer + 20
                                            else evaluateLeaf (Board size passes qs) aiPlayer - 20
                        else evaluateLeaf (Board size passes qs) aiPlayer

-- | Take a given branch, put the first layer of it back through to the exploreBranch method, which will loop back to this method until the maximum depth is reached and the score of a given board state at the end of the chain is found
makeFullBranch :: Int -- ^ The current depth
            -> [(Position, GameTree)] -- ^ list of Position-Tree pair representing the potential moves that can be made and their resultant possibilities
            -> Col -- ^ Colour of the AIPlayer
            -> Col -- ^ Colour of the current player
            -> [Int] -- ^ List of integers representing the scores of the respective paths
makeFullBranch _ [] _ _ = []
makeFullBranch depth ((pos,leafs):qs) aiPlayer currentPlayer = 
        exploreBranch (depth-1) leafs aiPlayer (other currentPlayer):makeFullBranch depth qs aiPlayer currentPlayer

-- | Loops through each given possible move to find what move is regarded as the "best" and then returns said move after the best move has been found by reaching the end of the list.
evaluateMoves :: Col -- ^ The colour of the current AI
                -> Board -- ^ Current board state
                -> [Position] -- ^ The list of valid moves the AI could make
                -> Position -- ^ The current best move
                -> Int -- ^ The evaluation score of the current best move
                -> Position -- ^ The best move according to this version of the AI
evaluateMoves colour board [] highestMove highestEvaluate = highestMove
evaluateMoves colour board (q:qs) highestMove highestEvaluate = do case makeMove board colour q of
                                                                        Nothing -> (0,0) --Should never be reached, used in safe manner
                                                                        Just board' -> do let potentialMax = evaluateBoard board' colour
                                                                                          if potentialMax <= highestEvaluate
                                                                                                then evaluateMoves colour board qs highestMove highestEvaluate
                                                                                                else evaluateMoves colour board qs q potentialMax

-- | Finds all possible moves that the current player could do
getPossible :: Board -- ^ The colour of the current player
            -> Col -- ^ Current board state
            -> [Position] -- ^ List of possible moves
getPossible board colour  = filter (\xpos -> isValid board xpos colour) [(xpos,ypos) | xpos <- [0..(size board -1)], ypos <- [0..(size board - 1)]]


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
getBestMoveOneDepth board colour = do let listOfPositions = getPossible board colour 
                                      evaluateMoves colour board listOfPositions (0,0) (-1)


-- | Returns a random move
getRandomMove :: Board -- ^ Current board state
                -> Col -- ^ Current player colour
                -> IO Position -- ^ Random move (must be IO due to random usage but is cleared in method one level higher)
getRandomMove board colour = do let listOfPositions = getPossible board colour
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
