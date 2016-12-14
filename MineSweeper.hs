module MineSweeper where

import DataTypes
import RunGame
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

-- Just an example game field.
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

-- Instance for an arbitrary game field
instance Arbitrary GameField where 
    arbitrary = do
        maxX <- elements [5..10]
        maxY <- elements [5..10]
        bombs <- elements [1..10]
        seed <- elements [1..999999]
        let gen = mkStdGen seed
        return (newGame maxX maxY bombs gen)

-- Generates a new game field with the given parameters
newGame :: Int -> Int -> Int -> StdGen -> GameField
newGame sizeX sizeY noBombs gen = addNumerics bombField bombPos
    where
        -- Start from an empty game field
        empty = emptyGameField sizeY sizeX
        -- Then add bombs at random positions
        bombPos = nRandPos gen noBombs [] (sizeY, sizeX)
        bombField = addBombs empty bombPos

-- Adds bombs to the given positions in the game field
addBombs :: GameField -> [Pos] -> GameField
addBombs gF [] = gF
addBombs (GameField rows) ((y,x):xs) = addBombs gF' xs
    where 
        gF' = GameField (rows !!= (y, rows !! y !!= (x, (Cell Closed Bomb))))

-- Updates the cells around all bombs to show correct number
addNumerics :: GameField -> [Pos] -> GameField
addNumerics gF []                           = gF
addNumerics (GameField rows) (pos:postail)  = addNumerics gF postail
    where
        gF = addNumerics' (GameField rows) $ calcOffsetPos (GameField rows) pos

addNumerics' :: GameField -> [Pos] -> GameField
addNumerics' gF [] = gF
addNumerics' (GameField rows) ((y,x):postail) =
    if val == Bomb then
        addNumerics' (GameField rows) postail
    else
        addNumerics' gF' postail
    where 
        (Cell state val) = rows !! y !! x
        (Numeric v) = val
        gF' = GameField (rows !!= (y, rows !! y !!= (x, (Cell state (Numeric(v+1))))))

-- Creates an empty game field of given dimensions
emptyGameField :: Int -> Int -> GameField
emptyGameField maxY maxX = GameField [ row | _ <- [0..maxY]]
    where 
        row = [Cell Closed (Numeric 0) | _ <- [0..maxX]]

-- Generates n random positions in the game field
nRandPos :: StdGen -> Int -> [Pos] -> Pos -> [Pos]
nRandPos _ 0 list _ = list 
nRandPos gen n list (maxY, maxX) = 
    if or [x == x' && y == y' | (y', x') <- list] then
        nRandPos gen'' n list (maxY, maxX)
    else 
        nRandPos gen'' (n-1) (list ++ [(y,x)]) (maxY, maxX)
    where
        (y, gen') = randomR (0, maxY) gen
        (x, gen'') = randomR (0, maxX) gen'

-- Flags a cell with the given position
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

-- Updates a list at the given index with the given value
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

-- Opens a cell at a given position if it isn't flagged.
clickCell :: GameField -> Pos -> GameField
clickCell (GameField rows) (y,x) =
    if (isOpened (Cell state v) || isFlagged (Cell state v)) then 
        (GameField rows)
    else 
        if (isEmptyCell (Cell state v)) then
            -- Recursively open neigboring cells if this cell is completely empty
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

prop_clickCell :: GameField -> Pos -> Property
prop_clickCell (GameField rows) (y,x) = 
    validPos && not (isOpened (Cell s v)) && not (isEmptyCell (Cell s v)) ==>
        s' == Opened && v' == v
    where 
        validPos = y >= 0 && y < length rows && x >= 0 && x < length (rows !! y)
        (Cell s v) = rows !! y !! x
        (GameField rows') = clickCell (GameField rows) (y,x)
        (Cell s' v') = rows' !! y !! x

-- Calculates all surrounding positions of a given coordinate
calcOffsetPos :: GameField -> Pos -> [Pos]
calcOffsetPos (GameField rows) (y,x) = 
    [(y'',x'') | (y',x') <- posOffset, let y'' = y+y', let x'' = x+x', y'' >= 0, y'' < yMax, x'' >= 0, x'' < xMax]
        where 
            yMax = length rows
            xMax = length $ rows !! 0 

-- Checks if all cells but those containing bombs are open
hasWon :: GameField -> Bool
hasWon (GameField rows) = 
    and [((state == Closed || state == Flagged) && value == Bomb) || 
            (state == Opened && value /= Bomb)  
        | row <-rows, (Cell state value) <- row] 

-- Checks if a bomb has been opened
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