module MineSweeper where

import DataTypes
import RunGame

example = GameField [
    [ec, ec, ec, ec, ec], 
    [e1, e1, e1, ec, ec],
    [e1, bo, e2, e1, ec],
    [e1, e2, bo, e1, ec],
    [ec, e1, e1, e1, ec]]
    where 
        ec = Cell Closed (Numeric 0)
        e1 = Cell Closed (Numeric 1)
        e2 = Cell Closed (Numeric 2)
        bo = Cell Closed Bomb

newGame :: GameField
newGame = example

flagCell :: GameField -> Pos -> GameField
flagCell (GameField rows) (y,x) = 
    if (isOpened (rows !! y !! x)) 
        then
            (GameField rows)
        else
            GameField (rows !!= (y, rows !! y !!= (x, (Cell Flagged v))))

    where
        (Cell _ v) = rows !! y !! x

(!!=) :: [a] -> (Int,a) -> [a]
list !!= (i, v) = [ if index == i then v else value | 
    (index, value) <- zip [0..] list]

isOpened :: Cell -> Bool
isOpened (Cell Opened _)    = True
isOpened _                  = False

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