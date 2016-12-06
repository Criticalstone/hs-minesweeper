module MineSweeper where

import DataTypes
import RunGame

newGame :: GameField
newGame = (GameField [[]])

flagCell :: GameField -> Pos -> GameField
flagCell _ _ = (GameField [[]])

clickCell :: GameField -> Pos -> GameField
clickCell _ _ = (GameField [[]])

hasWon :: GameField -> Bool
hasWon _ = False

gameOver :: GameField -> Bool
gameOver _ = False

implementation = Interface 
    {   iNewGame    = newGame
    ,   iFlagCell   = flagCell
    ,   iClickCell  = clickCell
    ,   iHasWon     = hasWon
    ,   iGameOver   = gameOver
    }