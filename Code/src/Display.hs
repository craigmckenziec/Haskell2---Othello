module Display where

import Board

import Control.Monad
import Data.Char
import Data.Int
import UI.NCurses

axisColour :: Curses ColorID
axisColour = do grey <- getGrey
                newColorID ColorBlack grey 1

backgroundColour :: Curses ColorID
backgroundColour = do grey <- getGrey
                      newColorID grey grey 2

blackColour :: Curses ColorID
blackColour = newColorID ColorBlack ColorBlack 3

boardColour :: Curses ColorID
boardColour = newColorID ColorGreen ColorGreen 4

getGrey :: Curses Color
getGrey = do let grey = Color (fromIntegral 1 :: Int16)
             defineColor grey 500 500 500
             return grey

whiteColour :: Curses ColorID
whiteColour = newColorID ColorWhite ColorWhite 5

drawGameState :: GameState -> Window -> Curses ()
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

drawBackground :: Window -> Int -> Curses ()
drawBackground w boardSize = do colourID <- backgroundColour
                                updateWindow w (setColor colourID)
                                forM_ [(x,y) | x<-[0..(getXCoord(boardSize))], y<-[0..(getYCoord(boardSize-1)+1)]] $ \(x,y) -> updateWindow w $ do moveCursor y x
                                                                                                                                                   drawString " "

drawXAxis :: Window -> Int -> Curses ()
drawXAxis w boardSize = do colourID <- axisColour
                           updateWindow w (setColor colourID)
                           forM_ [x | x <- [0..(boardSize-1)]] $ \x -> updateWindow w $ do moveCursor 0 (getXCoord x)
                                                                                           drawString [chr(65+x)]

drawYAxis :: Window -> Int -> Curses ()
drawYAxis w boardSize = do colourID <- axisColour
                           updateWindow w (setColor colourID)
                           forM_ [y | y <- [0..(min 9 (boardSize-1))]] $ \y -> updateWindow w $ do moveCursor (getYCoord y) 1
                                                                                                   drawString (show y)
                           when (boardSize-1 > 9) (do forM_ [y | y <- [10..(boardSize-1)]] $ \y -> updateWindow w $ do moveCursor (getYCoord y) 0
                                                                                                                       drawString (show y))

drawBoard :: Window -> Int -> Curses ()
drawBoard w boardSize = do colourID <- boardColour
                           updateWindow w (setColor colourID)
                           forM_ [(x,y) | x<-[0..(boardSize-1)], y<-[0..(boardSize-1)]] $ \(x,y) -> updateWindow w $ do moveCursor (getYCoord y) (getXCoord x)
                                                                                                                        drawString pieceString

drawPieces :: [(Position, Col)] -> Window -> Curses ()
drawPieces [] w = return ()
drawPieces (((x, y), colour):xs) w = do setPieceColour colour w
                                        updateWindow w $ do moveCursor (getYCoord y) (getXCoord x)
                                                            drawString pieceString
                                        drawPieces xs w

getXCoord :: Int -> Integer
getXCoord x = toInteger (3*x+3)

getYCoord :: Int -> Integer
getYCoord y = toInteger (2*y+2)

pieceString :: String
pieceString = "  "


setPieceColour :: Col -> Window -> Curses ()
setPieceColour Black w = do colourID <- blackColour
                            updateWindow w (setColor colourID)
setPieceColour White w = do colourID <- whiteColour
                            updateWindow w (setColor colourID)
