module RunGame where
import DataTypes

import Data.Char

data Interface = Interface 
    {   iNewGame    :: GameField 
    ,   iFlagCell   :: GameField -> Pos -> GameField
    ,   iClickCell  :: GameField -> Pos -> GameField
    ,   iHasWon     :: GameField -> Bool
    ,   iGameOver   :: GameField -> Bool
    }

printField :: GameField -> IO ()
printField (GameField rows) = 
            putStrLn (unlines [ [ cellToChar c | c <- row ] | row <- rows] )

cellToChar :: Cell -> Char
cellToChar (Cell Closed _)              = '.'
cellToChar (Cell Flagged _)             = 'P'
cellToChar (Cell Opened (Numeric 0))    = ' ' 
cellToChar (Cell _ (Numeric n))         = intToDigit n
cellToChar _                            = '+'  