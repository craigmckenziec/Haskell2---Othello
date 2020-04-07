module Board where

data Col = Black | White
  deriving (Show, Eq)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- Default board is 8x8, neither played has passed, with 4 initial pieces 
initBoard = Board 8 0 [((3,3), Black), ((3, 4), White),
                       ((4,3), White), ((4,4), Black)]

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, which is the computer player, timers, information about 
-- rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data GameState 
       = GameState { board :: Board,
                     turn :: Col }

initGameState = GameState initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove gameBoard colour (x,y) = 
         if checkPosition gameBoard (x,y) == True
            then Nothing
            else do let listWouldBeFlipped = postiionFlipsList gameBoard (x,y) colour
                    if length listWouldBeFlipped == 0
                        then Nothing 
                    else do let newPieces = (((x,y),colour): pieces gameBoard)
                            Just (Board 8 0 (flipFromPosition newPieces listWouldBeFlipped)) 


flipFromPosition :: [(Position, Col)] -> [Position] -> [(Position, Col)]
flipFromPosition currentPieces [] = currentPieces
flipFromPosition currentPieces (q:qs) = flipFromPosition (flipPiece currentPieces q) qs


flipPiece :: [(Position, Col)] -> Position -> [(Position, Col)]
flipPiece [] _ = [] -- Should never be reached as position is guaranteed to exist somewhere (done to prevent pattern matching errors)
flipPiece (((x,y), colour): qs) (searchX, searchY) = if x == searchX && y == searchY
                                                          then ((x,y), other colour):qs
                                                          else ((x,y), colour):(flipPiece qs (searchX, searchY))

-- Checks if position already has a piece
checkPosition :: Board -> Position -> Bool
checkPosition (Board size passes []) (x,y) = False
checkPosition (Board size passes (q:qs)) (x,y) 
    = (fst (fst q) == x && snd (fst q) == y) || checkPosition (Board size passes qs) (x,y)

-- returns a list of all of the positions that will be flipped if a piece of a specified colour is placed in a specified position
postiionFlipsList :: Board -> Position -> Col -> [Position]
postiionFlipsList gameBoard (x,y) colour = do let north = checkPath (x,y) gameBoard colour (0, -1) []
                                              let northEast = checkPath (x,y) gameBoard colour (1, -1) []
                                              let east = checkPath (x,y) gameBoard colour (1, 0) []
                                              let southEast = checkPath (x,y) gameBoard colour (1, 1) []
                                              let south = checkPath (x,y) gameBoard colour (0, 1) []
                                              let southWest = checkPath (x,y) gameBoard colour (-1, 1) []
                                              let west = checkPath (x,y) gameBoard colour (-1, 0) []
                                              let northWest = checkPath (x,y) gameBoard colour (-1, -1) []
                                              north ++ northEast ++ east ++ southEast ++ south ++ southWest ++ west ++ northWest

                                          
-- Checks a given path to see if it contains any possible flips
checkPath :: Position -> Board -> Col -> Position -> [Position] -> [Position]
checkPath (x, y) gameBoard colour (xChange, yChange) listOfFlips =
            do let xAdjusted = x + xChange
               let yAdjusted = y + yChange
               if checkPosition gameBoard (xAdjusted, yAdjusted) == True
                  then if getColour gameBoard (xAdjusted, yAdjusted) == colour
                    then listOfFlips
                    else checkPath (xAdjusted, yAdjusted) gameBoard colour (xChange, yChange) ((xAdjusted, yAdjusted):listOfFlips)
                else []


getColour :: Board -> Position -> Col
getColour (Board size passes []) (x,y) = Black -- Should never be reached (Function only for use when there is definitely a piece at that position) (done to prevent pattern matching errors)
getColour (Board size passes (q:qs)) (x,y) = 
           if fst(fst q) == x && snd (fst q) == y
             then snd q
             else getColour (Board size passes qs) (x,y)




-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore = undefined

-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver = undefined

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined
