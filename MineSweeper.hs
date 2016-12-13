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

posOffset = [(-1,-1), (-1,0), (-1,1),
             (0, -1),         ( 0,1),
             (1, -1), ( 1,0), ( 1,1)]

newGame :: GameField
newGame = example

flagCell :: GameField -> Pos -> GameField
flagCell (GameField rows) (y,x) = 
    if isOpened (rows !! y !! x) then
        (GameField rows)
    else if isFlagged (rows !! y !! x) then
        GameField (rows !!= (y, rows !! y !!= (x, (Cell Closed v))))
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

isFlagged :: Cell -> Bool
isFlagged (Cell Flagged _)  = True
isFlagged _                 = False

isEmptyCell :: Cell -> Bool
isEmptyCell (Cell _ (Numeric 0)) = True
isEmptyCell _                    = False 

clickCell :: GameField -> Pos -> GameField
clickCell (GameField rows) (y,x) =
    if (isOpened (Cell state v) || isFlagged (Cell state v)) then 
        (GameField rows)
    else 
        if (isEmptyCell (Cell state v)) then
            clickCell' clickedGf (calcOffsetPos clickedGf (y,x))
        else
            clickedGf
    where 
        clickedGf       = (GameField (rows !!= (y, rows !! y !!= (x, (Cell Opened v)))))
        (Cell state v) = rows !! y !! x

clickCell' :: GameField -> [Pos] -> GameField
clickCell' gf [pos]         = clickCell gf pos
clickCell' gf (pos:posxs)   = clickCell' gf' posxs
    where
        gf' = clickCell gf pos

calcOffsetPos :: GameField -> Pos -> [Pos]
calcOffsetPos (GameField rows) (y,x) = 
    [(y'',x'') | (y',x') <- posOffset, let y'' = y+y', let x'' = x+x', y'' >= 0, y'' < yMax, x'' >= 0, x'' < xMax]
        where 
            yMax = length rows
            xMax = length $ rows !! 0 

hasWon :: GameField -> Bool
hasWon (GameField rows) = 
    and [((state == Closed || state == Flagged) && value == Bomb) || 
            (state == Opened && value /= Bomb)  
        | row <-rows, (Cell state value) <- row] 

gameOver :: GameField -> Bool
gameOver (GameField rows) = or [ state == Opened && value == Bomb 
    | row <- rows, (Cell state value) <- row]

implementation = Interface 
    {   iNewGame    = newGame
    ,   iFlagCell   = flagCell
    ,   iClickCell  = clickCell
    ,   iHasWon     = hasWon
    ,   iGameOver   = gameOver
    }

main :: IO ()
main = runGame implementation