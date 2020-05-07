Welcome to Othello!

Usage instructions --

*All usage instructions assume that the user is within the src directory unless otherwise stated*

To play a default game (Human vs. Standard AI, size of 8, hints on):
                        ./Main

To play a game of your own settings, follow the usage pattern:
                        ./Main <BlackPlayerType> ("AI" || "AdvancedAI" || "Human") <WhitePlayerType> ("AI" || "AdvancedAI" || "Human") <Size> (8..26) <GameMode> ("Othello" || "Reversi") <HintsToggle> ("On" || "Off")

To play a savegame, follow the usage pattern:
                        ./Main <filename>.txt

The list of instructions while playing a match are as follows :
                        Options Menu: Escape key
                        Pass on turn: Special character "+"
                        Undo a move: Special character "-"
                        Save a game (by default to savegame.txt): Special character ">"


-- End usage instructions

NOTE: If a user prefers to use the cabal build option rather than using ghc, they must do the following,

Use the command "cabal build" within the Code directory

Navigate to the Code/dist/build/othello directory

From here you can use any of the instructions above, but replace the word "Main" with "othello"

END NOTE