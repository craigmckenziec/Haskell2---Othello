{-|
Module : Board
Description : Handles operations directly concerning the Board datatype or its contents
-}
module Board where

-- | Col contains the colours that the player can be, White or Black and a possible ColourError used for when reading the colour in from somewhere
data Col = Black | White | ColourError
  deriving (Show, Eq, Ord)

-- | PlayerType contains the types that the player can be, AI or Human and a possible PLayerTypeError used for when reading the player in from somewhere
data PlayerType = AI | Human | AdvancedAI | PLayerTypeError
  deriving (Show, Eq)

-- | GameMode contains the modes that the game can be, Othello or Reversi and a possible GameModeError used for when reading the mode in from somewhere
data GameMode = Othello | Reversi | GameModeError
  deriving (Show, Eq)

-- | HintsToggle contains the toggle that the hint option can be, On or Off and a possible HintsToggleError used for when reading the option in from somewhere
data HintsToggle = On | Off | HintsToggleError
  deriving (Show, Eq)

-- | Function that will change the colour inputted (used for swapping players)
other :: Col -- ^ Current colour
      -> Col -- ^ New colour
other Black = White
other White = Black

-- | Position type that uses two ints to represent a coordinate
type Position = (Int, Int)

-- | A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.
data Board = Board { size :: Int, -- ^ The size of the current board
                     passes :: Int, -- ^ The current number of passes
                     pieces :: [(Position, Col)] -- ^ The current Pieces present on the board
                   }
  deriving Show

-- | Default board is 8x8, neither played has passed, with 4 initial pieces
initBoard = Board 8 0 [((3,3), Black), ((3, 4), White),
                       ((4,3), White), ((4,4), Black)]

-- | Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, which is the computer player, timers, information about
-- rule variants, etc)
data GameState
       = GameState { board :: Board, -- ^ A given board state
                     turn :: Col, -- ^ The current turn (represented by a colour)
                     blackPlayer :: PlayerType, -- ^ The Type of the Black Player
                     whitePlayer :: PlayerType, -- ^ The Type of the White Player
                     gameMode :: GameMode, -- ^ The Mode the game is in
                     hintsToggle :: HintsToggle, -- ^ Whether or not hints are currently working
                     previousBoards :: [Board] -- ^ A list of previous boards to facilitate the use of the undo command
                     }

-- | Generates a default GameState for use when no options are given by the user
initGameState = GameState initBoard Black Human AI Othello On []

-- | Function that takes in a given move and in the context of the current board will either return the board, now updated if 
--  the move was valid or Nothing if the move was invalid
makeMove :: Board -- ^ Current board state
          -> Col -- ^ Current player colour
          -> Position -- ^ Move to be investigated
          -> Maybe Board -- ^ Nothing if invalid move, updated board object if valid
makeMove gameBoard colour (x,y) =
         if checkPosition gameBoard (x,y) == True || (x >= size gameBoard || y >= size gameBoard)
            then Nothing
            else do let listWouldBeFlipped = positionFlipsList gameBoard (x,y) colour
                    if length listWouldBeFlipped == 0
                        then Nothing
                    else do let newPieces = (((x,y),colour): pieces gameBoard)
                            Just (Board (size gameBoard) 0 (flipFromPosition newPieces listWouldBeFlipped))

-- | Flips positions that should be flipped by action of a move being made
flipFromPosition :: [(Position, Col)] -- ^ List of pieces that currently exist
                  -> [Position] -- ^ Positions to be flipped
                  -> [(Position, Col)] -- ^ New Pieces list with positions flipped
flipFromPosition currentPieces [] = currentPieces
flipFromPosition currentPieces (q:qs) = flipFromPosition (flipPiece currentPieces q) qs

-- | Flips a given position in the list of pieces
flipPiece :: [(Position, Col)] -- ^ List of pieces
            -> Position -- ^ Position to be flipped
            -> [(Position, Col)] -- ^ New list of pieces with position flipped
flipPiece [] _ = [] -- Should never be reached as position is guaranteed to exist somewhere (done to prevent pattern matching errors)
flipPiece (((x,y), colour): qs) (searchX, searchY) = if x == searchX && y == searchY
                                                          then ((x,y), other colour):qs
                                                          else ((x,y), colour):(flipPiece qs (searchX, searchY))

-- | Checks if position already has a piece
checkPosition :: Board -- ^  Current board state
              -> Position -- ^  Position to be investigated
              -> Bool -- ^  True if position is occupied, false if empty
checkPosition (Board size passes []) (x,y) = False
checkPosition (Board size passes (q:qs)) (x,y)
    = (fst (fst q) == x && snd (fst q) == y) || checkPosition (Board size passes qs) (x,y)

-- | returns a list of all of the positions that will be flipped if a piece of a specified colour is placed in a specified position
positionFlipsList :: Board -- ^ Current board state
                  -> Position -- ^ Position to be investigated
                  -> Col -- ^ Current player colour
                  -> [Position] -- ^ List of pieces that would be flipped
positionFlipsList gameBoard (x,y) colour = do let north = checkPath (x,y) gameBoard colour (0, -1) []
                                              let northEast = checkPath (x,y) gameBoard colour (1, -1) []
                                              let east = checkPath (x,y) gameBoard colour (1, 0) []
                                              let southEast = checkPath (x,y) gameBoard colour (1, 1) []
                                              let south = checkPath (x,y) gameBoard colour (0, 1) []
                                              let southWest = checkPath (x,y) gameBoard colour (-1, 1) []
                                              let west = checkPath (x,y) gameBoard colour (-1, 0) []
                                              let northWest = checkPath (x,y) gameBoard colour (-1, -1) []
                                              north ++ northEast ++ east ++ southEast ++ south ++ southWest ++ west ++ northWest


-- | Checks a given path to see if it contains any possible flips
checkPath :: Position -- ^ Position to be investigated from
          -> Board -- ^ Current board state
          -> Col -- ^ Current player colour
          -> Position -- ^ The direction in which to check
          -> [Position] -- ^ List of flips generated so far
          -> [Position] -- ^ Complete list of flips
checkPath (x, y) gameBoard colour (xChange, yChange) listOfFlips =
            do let xAdjusted = x + xChange
               let yAdjusted = y + yChange
               if checkPosition gameBoard (xAdjusted, yAdjusted) == True
                  then if getColour gameBoard (xAdjusted, yAdjusted) == colour
                    then listOfFlips
                    else checkPath (xAdjusted, yAdjusted) gameBoard colour (xChange, yChange) ((xAdjusted, yAdjusted):listOfFlips)
                else []

-- | Get the colour of a list at a given position
getColour :: Board -- ^ Current board state
          -> Position -- ^ Position to check the colour of
          -> Col -- ^ Colour of the piece at given position
getColour (Board size passes []) (x,y) = Black -- Should never be reached (Function only for use when there is definitely a piece at that position) (done to prevent pattern matching errors)
getColour (Board size passes (q:qs)) (x,y) =
           if fst(fst q) == x && snd (fst q) == y
             then snd q
             else getColour (Board size passes qs) (x,y)

-- | Play a reversi placement on the board; return 'Nothing' if the move is invalid
makeReversiInitialMove :: Board -- ^ Current board state 
                        -> Col -- ^ Current player colour
                        -> Position -- ^ Move to be checked
                        -> Maybe Board -- ^ Nothing if placement was invalid, updated board if placement was valid
makeReversiInitialMove gameBoard colour (x,y) =
      do let midpoint = div (size gameBoard) 2
         if checkPosition gameBoard (x,y) == True || x < midpoint - 1 || x > midpoint || y < midpoint - 1 || y > midpoint
            then Nothing
            else do let newPieces = (((x,y),colour): pieces gameBoard)
                    Just (Board (size gameBoard) 0 newPieces)

-- | Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -- ^ Current board state
          -> Bool -- ^ True if game is complete, false if game is incomplete
gameOver gameBoard = if (passes gameBoard) == 2 || length(pieces gameBoard) == ((size gameBoard) ^ 2)
                            then True
                            else False

-- | Evaluate the given board for a player by checking the number of pieces they currently have on the board
evaluateBoard :: Board -- ^ Current board state
              -> Col -- ^ Colour to be investigated
              -> Int -- ^ Number of pieces that player currently has on board
evaluateBoard (Board size passes []) searchColour = 0
evaluateBoard (Board size passes ((_, currentColour): qs)) searchColour =
            if currentColour == searchColour then evaluateBoard (Board size passes qs) searchColour + 1
            else evaluateBoard (Board size passes qs) searchColour
