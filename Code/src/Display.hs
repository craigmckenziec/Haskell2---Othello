{-|
Module : Display
Description : Handles operations directly concerning the building of the NCurses display to the user
-}
module Display where

import Board

import Control.Monad
import Data.Char
import Data.Int
import UI.NCurses

-- | Return the colour to build the Axis for the ncurses display
axisColour :: Curses ColorID -- ^ Colour to build the axis as
axisColour = do grey <- getGrey
                newColorID ColorBlack grey 1

-- | Return the colour to build the background for the ncurses display
backgroundColour :: Curses ColorID -- ^ Colour to build the background as
backgroundColour = do grey <- getGrey
                      newColorID grey grey 2

-- | Return the colour to display the blackPlayer as 
blackColour :: Curses ColorID -- ^ Colour to build the blackPlayer as
blackColour = newColorID ColorBlack ColorBlack 3

-- | Return the colour to display the empty slots as
boardColour :: Curses ColorID -- ^ Colour to build the empty slots as
boardColour = newColorID ColorGreen ColorGreen 4

-- | Get an appropriate Grey colour
getGrey :: Curses Color -- ^ Colour that represents grey
getGrey = do let grey = Color (fromIntegral 1 :: Int16)
             defineColor grey 500 500 500
             return grey

-- | Return the colour to display the whitePlayer as 
whiteColour :: Curses ColorID -- ^ Colour to build the whitePlayer as
whiteColour = newColorID ColorWhite ColorWhite 5

-- | Displays the current GameState to the terminal, showing the current board, who's turn it is and giving a prompt for the user to enter their move
drawGameState :: GameState -- ^ Current GameState
                -> Window -- ^ Window object to display onto
                -> Curses () -- ^ Curses wrapper to return to
drawGameState st w = do let boardSize = size (board st)
                        updateWindow w clear
                        drawBackground w boardSize
                        drawXAxis w boardSize
                        drawYAxis w boardSize
                        drawBoard w boardSize
                        drawPieces (pieces (board st)) w
                        updateWindow w $ do setColor defaultColorID
                                            moveCursor (toInteger (getYCoord(boardSize))) 0
                                            drawString ((show (turn st)) ++ "'s turn\n")
                                            drawString ("move: ")
                        render

-- | Draw a background to display the current board onto
drawBackground :: Window -- ^ Window object to display onto
                -> Int -- ^ Current board size
                -> Curses () -- ^ Curses wrapper to return to
drawBackground w boardSize = do colourID <- backgroundColour
                                updateWindow w (setColor colourID)
                                forM_ [(x,y) | x<-[0..(getXCoord(boardSize))], y<-[0..(getYCoord(boardSize-1)+1)]] $ \(x,y) -> updateWindow w $ do moveCursor y x
                                                                                                                                                   drawString " "

-- | Draw a single XAxis onto the window object
drawXAxis :: Window -- ^ Window object to display onto
                -> Int -- ^ Current board size
                -> Curses () -- ^ Curses wrapper to return to
drawXAxis w boardSize = do colourID <- axisColour
                           updateWindow w (setColor colourID)
                           forM_ [x | x <- [0..(boardSize-1)]] $ \x -> updateWindow w $ do moveCursor 0 (getXCoord x)
                                                                                           drawString [chr(65+x)]
-- | Draw a single YAxis onto the window object
drawYAxis :: Window -- ^ Window object to display onto
                -> Int -- ^ Current board size
                -> Curses () -- ^ Curses wrapper to return to
drawYAxis w boardSize = do colourID <- axisColour
                           updateWindow w (setColor colourID)
                           forM_ [y | y <- [0..(min 9 (boardSize-1))]] $ \y -> updateWindow w $ do moveCursor (getYCoord y) 1
                                                                                                   drawString (show y)
                           when (boardSize-1 > 9) (do forM_ [y | y <- [10..(boardSize-1)]] $ \y -> updateWindow w $ do moveCursor (getYCoord y) 0
                                                                                                                       drawString (show y))

-- | Draw an empty board onto the current window object
drawBoard :: Window -- ^ Window object to display onto
                -> Int -- ^ Current board size
                -> Curses () -- ^ Curses wrapper to return to
drawBoard w boardSize = do colourID <- boardColour
                           updateWindow w (setColor colourID)
                           forM_ [(x,y) | x<-[0..(boardSize-1)], y<-[0..(boardSize-1)]] $ \(x,y) -> updateWindow w $ do moveCursor (getYCoord y) (getXCoord x)
                                                                                                                        drawString pieceString

-- | Draws a list of pieces onto the window object
drawPieces :: [(Position, Col)] -- ^ List of pieces
            -> Window -- ^ Window object to display onto
            -> Curses () -- ^ Curses wrapper to return to
drawPieces [] w = return ()
drawPieces (((x, y), colour):xs) w = do setPieceColour colour w
                                        updateWindow w $ do moveCursor (getYCoord y) (getXCoord x)
                                                            drawString pieceString
                                        drawPieces xs w

-- | Convert an int object into an integer object for use on the X Axis
getXCoord :: Int -- ^ Given int
            -> Integer -- ^ Conversion to integer
getXCoord x = toInteger (3*x+3)

-- | Convert an int object into an integer object for use on the X Axis
getYCoord :: Int -- ^ Given int
            -> Integer -- ^ Conversion to integer
getYCoord y = toInteger (2*y+2) 

-- | Return a piece String that represents a single slot
pieceString :: String -- Slot string that is returned
pieceString = "  "

-- | set a piece to a certain colourID depedning on the Colour the piece is on the board 
setPieceColour :: Col -- ^ Colour to be searched for (Black or White)
                -> Window  -- ^ Window object to display onto
                -> Curses () -- ^ Curses wrapper to return to
setPieceColour Black w = do colourID <- blackColour
                            updateWindow w (setColor colourID)
setPieceColour White w = do colourID <- whiteColour
                            updateWindow w (setColor colourID)
